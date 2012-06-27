-module(ftpd_ctrl_conn).

-export([new_connection/3]).

-include_lib("inets/src/inets_app/inets_internal.hrl").

-include_lib("inets/include/ftpd.hrl").
-include_lib("ftpd_rep.hrl").

%%
%% FTP control connection for handling commands
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Connect and process messages
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

new_connection(Sock, SupPid, Args) ->
	erlang:monitor(process, SupPid),
	io:format("---------------- CONNECTION START ----------------\n"),
	send_reply(Sock, 220, "Hello"),
	ConnData = construct_conn_data(Args, Sock),
	do_recv(Sock, ConnData).

construct_conn_data(Args, Sock) ->
	ErlTop = element(2,file:get_cwd()),
	#ctrl_conn_data{
		control_socket = Sock, 
		chrootdir      = proplists:get_value(chrootDir, Args, ErlTop), %% HINT chrootDir vs chrootdir
		pwd_fun        = proplists:get_value(pwd_fun,   Args, fun(_,_) -> not_authorized end),
		log_fun        = proplists:get_value(log_fun,   Args, fun(_,_) -> ok end),
		trace_fun      = proplists:get_value(trace_fun, Args, fun(_,_) -> ok end)
	}.

%% Control Connection - Wait for incoming messages
-spec do_recv(Sock :: socket(), Args :: connstate()) -> ok.
do_recv(Sock, Args) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, Data} ->
			{Command, Msg} = ?UTIL:packet_to_tokens(Data),
			io:format("[~p-Recv]: ~p - ~p\n", [self(), Command, Msg]),
			NewArgs = process_message(Sock, Command, Msg, Args),
			after_reply(Sock, Command, NewArgs);
        {error, closed} -> ok
    end.

process_message(Sock, Command, Msg, Args) ->
	case ?UTIL:check_auth(Command, Args) of
		ok ->
			{Reply, MaybeNewArgs} = handle_command(Command, Msg, Args),
			handle_reply(Sock, Reply),
			case MaybeNewArgs of
				{newargs, NewArgs} -> NewArgs;
				sameargs           -> Args
			end;
		bad ->
			handle_reply(Sock, ?UTIL:response(530, "Please login with USER and PASS")),
			Args
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Handle incoming FTP commands
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec handle_command(Command :: string(), Message :: list(), Args :: connstate()) -> {reply(), argschange()}.
handle_command(<<"NOOP">>, _, _) ->
	{?UTIL:response(200, "NOOP command successful"), sameargs};

handle_command(<<"QUIT">>, _, Args) ->
	User = Args#ctrl_conn_data.username,
	?UTIL:logf(Args, ?CONN_CLOSE, [User]),
	{?UTIL:response(221, "Goodbye."), sameargs};

handle_command(<<"USER">>, [UserBin|_], Args) ->
	User = binary_to_list(UserBin),
	case Args#ctrl_conn_data.authed of
		true -> 
			{?UTIL:response(503, "You are already logged in"), sameargs};
		false ->
			NewArgs = Args#ctrl_conn_data{ username = User },
			{?UTIL:response(331, "Password required for " ++ User), {newargs, NewArgs}}
	end;

handle_command(<<"PASS">>, [PasswordBin|_], Args) ->
	Authed = Args#ctrl_conn_data.authed,
	User   = Args#ctrl_conn_data.username,
	Password = binary_to_list(PasswordBin),
	case {Authed, User} of
		{false, none} -> {?UTIL:response(503, "Login with USER first"),     sameargs};
		{false, _} ->
			PwdFun = Args#ctrl_conn_data.pwd_fun,
			case PwdFun(User, Password) of
				authorized ->
					?UTIL:logf(Args, ?LOGIN_OK, [User]),
					NewArgs = Args#ctrl_conn_data{ authed = true },
					{?UTIL:response(230, "Login ok - TODO: not sure"), {newargs, NewArgs}};
				not_authorized ->
					?UTIL:logf(Args, ?LOGIN_FAIL, [User]),
					NewArgs = Args#ctrl_conn_data{ username = none },
					{?UTIL:response(530, "Login incorrect"), {newargs, NewArgs}}
			end;
		{true, _}     -> {?UTIL:response(503, "You are already logged in"), sameargs}
	end;

handle_command(<<"TYPE">>, ParamsBin, Args) ->
	Params  = [ binary_to_list(E)  || E <- ParamsBin],	%% TEMP
	ParamsF = [ string:to_upper(E) || E <- Params],
	case ?UTIL:check_repr_type(ParamsF) of
		true ->
			NewArgs = Args#ctrl_conn_data{ repr_type = ParamsF },
			{?UTIL:response(200, "TYPE set to " ++ hd(ParamsF)), {newargs, NewArgs}};
		false ->
			{?UTIL:response(500, "'TYPE " ++ string:join(Params, " ") ++ "' not understood"), sameargs}
	end;

handle_command(<<"RETR">>, ParamsBin, Args) ->
	Params = [ binary_to_list(E) || E <- ParamsBin],	%% TEMP
	FileName = string:join(Params, " "),
	ftpd_data_conn:send_msg(retr, FileName, Args);

handle_command(<<"STOR">>, ParamsBin, Args) ->
	Params = [ binary_to_list(E) || E <- ParamsBin],	%% TEMP
	FullName = string:join(Params, " "),
	FileName = filename:basename(FullName) ++ filename:extension(FullName),
	ftpd_data_conn:send_msg(stor, {FileName, FullName}, Args);

handle_command(<<"CWD">>, ParamsBin, Args) ->
	Params = [ binary_to_list(E) || E <- ParamsBin],	%% TEMP
	NewDir = string:join(Params, " "),
	CurDir = Args#ctrl_conn_data.curr_path,
	BaseDir = Args#ctrl_conn_data.chrootdir,
	case ftpd_dir:set_cwd(BaseDir, CurDir, NewDir) of
		{ok, NewPath} ->
			?UTIL:tracef(Args, ?CWD, [NewPath -- "/"]),
			io:format("CWD new path: ~p", [NewPath]),
			NewArgs = Args#ctrl_conn_data{ curr_path = NewPath },
			{?UTIL:response(250, "CWD command successful."), {newargs, NewArgs}};
		{error, Error} ->
			io:format("CWD error: ~p", [Error]),
			{?UTIL:response(550, NewDir ++ ": No such file or directory"), sameargs}
	end;

handle_command(<<"PWD">>, [], Args) ->
	{?UTIL:response(257, "\"" ++ Args#ctrl_conn_data.curr_path ++ "\" is the current directory"), sameargs};

handle_command(<<"PWD">>, _, _) ->	% TODO: generalize
	{?UTIL:response(501, "Invalid number of arguments"), sameargs};

handle_command(<<"PASV">>, _, Args) ->
	{ok, Hostname} = inet:gethostname(),
	case inet:getaddr(Hostname, inet) of
		{ok, Address} ->
			ftpd_data_conn:reinit_passive_conn(Args#ctrl_conn_data.pasv_pid),
			{PasvPid, {ok, Port}} = ftpd_data_conn:start_passive_mode(inet4),
			io:format("Passive mode start, port: ~p\n", [Port]),
			NewArgs = Args#ctrl_conn_data{ pasv_pid = PasvPid },
			{?UTIL:response(227, "Entering Passive Mode (" ++ ?UTIL:format_address(Address, Port) ++ ")."), {newargs, NewArgs}};
		{error, Error} ->
			io:format("ERROR: inet:getaddr, ~p\n", [Error]),
			{?UTIL:response(500, "PASV command failed"), sameargs}
	end;

handle_command(<<"EPSV">>, _, Args) ->
	ftpd_data_conn:reinit_passive_conn(Args#ctrl_conn_data.pasv_pid),
	{PasvPid, {ok, Port}} = ftpd_data_conn:start_passive_mode(inet6),
	io:format("Passive mode start, port: ~p\n", [Port]),
	NewArgs = Args#ctrl_conn_data{ pasv_pid = PasvPid },
	{?UTIL:response(229, "Entering Extended Passive Mode (|||" ++ integer_to_list(Port) ++ "|)"), {newargs, NewArgs}};

handle_command(<<"PORT">>, [_Address], _) ->
	{?UTIL:response(500, "TODO"), sameargs};

handle_command(<<"PORT">>, _, _) ->
	{?UTIL:response(501, "Illegal PORT command"), sameargs};

handle_command(<<"LIST">>, ParamsBin, Args) -> %% TODO move to data_conn
	Params = [ binary_to_list(E) || E <- ParamsBin],	%% TEMP
	DirToList = string:join(Params, " "),
	AbsPath = Args#ctrl_conn_data.chrootdir,
	RelPath = Args#ctrl_conn_data.curr_path,
	case ftpd_dir:set_cwd(AbsPath, RelPath, DirToList) of
		{ok, NewPath} ->
			?UTIL:tracef(Args, ?LIST, [NewPath]),
			FullPath = AbsPath ++ "/" ++ RelPath ++ DirToList,
			io:format("LIST path: ~p\n", [FullPath]),
			{ok, FileNames} = file:list_dir(AbsPath ++ NewPath),
			ftpd_data_conn:send_msg(list, {lists:sort(FileNames), FullPath}, Args);
		{error, Error} ->
			io:format("LIST error: ~p  | ~p | ~p | ~p | ~p\n", 
									[Error,Params,DirToList,AbsPath,RelPath]),
			{?UTIL:response(550, "LIST fail TEMP TODO"), sameargs}
	end;

handle_command(<<"">>, _, _) ->
	{?UTIL:response(500, "Invalid command: try being more creative"), sameargs};

handle_command(Command, _, _) ->
	{?UTIL:response(500, binary_to_list(Command) ++ " not implemented"), sameargs}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Control functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Send response if needed
handle_reply(_, noreply) ->
	ok;
handle_reply(Sock, {reply, Code, Message}) ->
	send_reply(Sock, Code, Message).

%% Convert Code and Message to packet and send
send_reply(Sock, Code, Message) ->
	io:format("[~p-Send]: ~p - ~p\n", [self(), Code, Message]),
	Str = integer_to_list(Code) ++ " " ++ Message ++ "\r\n",
	gen_tcp:send(Sock, Str).

%% Control behaviour after replying to a message
after_reply(Sock, <<"QUIT">>, _) ->
	io:format("---------------- CONNECTION CLOSE ----------------\n"),
	gen_tcp:close(Sock);
after_reply(Sock, _, Args) ->
	do_recv(Sock, Args).
