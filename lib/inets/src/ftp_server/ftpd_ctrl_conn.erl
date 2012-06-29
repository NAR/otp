%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%
%%

-module(ftpd_ctrl_conn).

-export([new_connection/2]).

-include_lib("inets/include/ftpd.hrl").
-include_lib("ftpd_rep.hrl").

%%
%% FTP control connection for handling commands
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Connect and process messages
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

new_connection(Sock, Args) ->
   	SupPid = proplists:get_value(sup_pid, Args),
	erlang:monitor(process, SupPid),

	io:format("---------------- CONNECTION START ----------------\n"),
	?UTIL:send_reply(Sock, 220, "Hello"),

	ConnData = construct_conn_data(Args, Sock),
	do_recv(Sock, ConnData).

construct_conn_data(Args, Sock) ->
	ErlTop  = element(2, file:get_cwd()),
	RootDir = proplists:get_value(chrootDir, Args, ErlTop),
	BaseDir = re:replace(RootDir, "\/$", "", [{return, list}]), %% trim / at end
	#ctrl_conn_data{
		control_socket = Sock,
		chrootdir = BaseDir,
		pwd_fun   = proplists:get_value(pwd_fun,   Args,
						fun(_,_) -> not_authorized end),
		log_fun   = proplists:get_value(log_fun,   Args, fun(_,_) -> ok end),
		trace_fun = proplists:get_value(trace_fun, Args, fun(_,_) -> ok end)
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
			handle_reply(Sock, ?RESP(530, "Please login with USER and PASS")),
			Args
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Handle incoming FTP commands
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec handle_command(Command :: bitstring(), Message :: [bitstring()],
	Args :: connstate()) -> {reply(), argschange()}.

handle_command(<<"NOOP">>, _, _) ->
	mk_rep(200, "NOOP command successful");

handle_command(<<"QUIT">>, _, Args) ->
	User = Args#ctrl_conn_data.username,
	?UTIL:logf(Args, ?CONN_CLOSE, [User]),
	mk_rep(221, "Goodbye.");

handle_command(<<"USER">>, ParamsBin, Args) ->
	User = ?UTIL:binlist_to_string(ParamsBin),
	case Args#ctrl_conn_data.authed of
		true  ->
			mk_rep(503, "You are already logged in");
		false ->
			NewArgs = Args#ctrl_conn_data{ username = User },
			mk_rep(331, "Password required for " ++ User, NewArgs)
	end;

handle_command(<<"PASS">>, ParamsBin, Args) ->
	Password = ?UTIL:binlist_to_string(ParamsBin),
	Authed   = Args#ctrl_conn_data.authed,
	User     = Args#ctrl_conn_data.username,
	case {Authed, User} of
		{true,  _}    -> mk_rep(503, "You are already logged in");
		{false, none} -> mk_rep(503, "Login with USER first");
		{false, _} ->
			PwdFun = Args#ctrl_conn_data.pwd_fun,
			case PwdFun(User, Password) of
				authorized ->
					?UTIL:logf(Args, ?LOGIN_OK, [User]),
					NewArgs = Args#ctrl_conn_data{ authed = true },
					mk_rep(230, "User logged in, proceed.", NewArgs);
				not_authorized ->
					?UTIL:logf(Args, ?LOGIN_FAIL, [User]),
					NewArgs = Args#ctrl_conn_data{ username = none },
					mk_rep(530, "Login incorrect", NewArgs)
			end
	end;

handle_command(<<"TYPE">>, ParamsBin, Args) ->
	Params  = [ binary_to_list(E)  || E <- ParamsBin],	%% TEMP
	ParamsF = [ string:to_upper(E) || E <- Params],
	io:write(typeset),
	case ?UTIL:check_repr_type(ParamsF) of
		true ->
			NewArgs = Args#ctrl_conn_data{ repr_type = ParamsF },
			mk_rep(200, "TYPE set to " ++ hd(ParamsF), NewArgs);
		false ->
			mk_rep(500, "'TYPE "++string:join(Params," ")++"' not understood")
	end;

handle_command(<<"SIZE">>, ParamsBin, Args) ->
	FileName = ?UTIL:binlist_to_string(ParamsBin),
	FullPath = ?UTIL:get_full_path(Args) ++ FileName,
	case filelib:is_regular(FullPath) of
		true  -> mk_rep(213, integer_to_list(filelib:file_size(FullPath)));
		false -> mk_rep(550, FileName ++ ": not a regular file")
	end;

handle_command(<<"RETR">>, ParamsBin, Args) ->
	FileName = ?UTIL:binlist_to_string(ParamsBin),
	ftpd_data_conn:send_msg(retr, FileName, Args);

handle_command(<<"STOR">>, ParamsBin, Args) ->
	FullName = ?UTIL:binlist_to_string(ParamsBin),
	FileName = ?UTIL:get_file_name(FullName),
	ftpd_data_conn:send_msg(stor, {FileName, FullName, write}, Args);

handle_command(<<"APPE">>, ParamsBin, Args) ->
	FullName = ?UTIL:binlist_to_string(ParamsBin),
	FileName = ?UTIL:get_file_name(FullName),
	ftpd_data_conn:send_msg(stor, {FileName, FullName, append}, Args);

handle_command(<<"CWD">>, ParamsBin, Args) ->
	NewDir  = ?UTIL:binlist_to_string(ParamsBin),
	CurDir  = Args#ctrl_conn_data.curr_path,
	BaseDir = Args#ctrl_conn_data.chrootdir,
	case ftpd_dir:set_cwd(BaseDir, CurDir, NewDir) of
		{ok, NewPath} ->
			NewPath2 =					%% TODO ??
				case NewPath of
					""      -> "/";
					NewPath -> NewPath
				end,
			?UTIL:tracef(Args, ?CWD, [NewPath2]),
			io:format("CWD new path: ~p", [NewPath2]),
			NewArgs = Args#ctrl_conn_data{ curr_path = NewPath2 },
			mk_rep(250, "CWD command successful.", NewArgs);
		{error, Error} ->
			io:format("CWD error: ~p", [Error]),
			mk_rep(550, NewDir ++ ": No such file or directory")
	end;

handle_command(<<"PWD">>, [], Args) ->
	RspStr = "\""++Args#ctrl_conn_data.curr_path++"\" is the current directory",
	mk_rep(257, RspStr);

handle_command(<<"PWD">>, _, _) ->	% TODO: generalize
	mk_rep(501, "Invalid number of arguments");

handle_command(<<"STRU">>, [Type], _) ->
	case ?UTIL:bin_to_upper(Type) of
		<<"F">> -> mk_rep(200, "Structure set to F");
		_		-> mk_rep(504, "Unsupported structure type")
	end;

handle_command(<<"PASV">>, _, Args) ->
	case ?UTIL:get_server_ip() of
		{ok, Address} ->
			ftpd_data_conn:reinit_data_conn(Args),
			{PasvPid, {ok, Port}} = ftpd_data_conn:start_passive_mode(inet4),
			NewArgs = Args#ctrl_conn_data{ data_pid = PasvPid },
			AddrStr = ?UTIL:format_address(Address, Port),
			mk_rep(227, "Entering Passive Mode (" ++ AddrStr ++ ").", NewArgs);
		{error, Error} ->
			io:format("ERROR: inet:getaddr, ~p\n", [Error]),
			mk_rep(500, "PASV command failed")
	end;

handle_command(<<"PORT">>, [BinArg1], Args) ->
	Params       = binary_to_list(BinArg1),
	IpPortParams = string:tokens(Params,","),
	case ?UTIL:list2portip(IpPortParams) of
		{error, _}   -> mk_rep(500, "PORT command failed (2)");
		{Addr, Port} ->
			ftpd_data_conn:reinit_data_conn(Args),
			case ftpd_data_conn:start_active_mode(inet4, Addr, Port) of
				{ok, PasvPid} ->
					NewArgs = Args#ctrl_conn_data{ data_pid = PasvPid },
					mk_rep(200, "PORT command successful", NewArgs);
				{error, _} -> mk_rep(500, "PORT command failed (1)")
			end
	end;

handle_command(<<"PORT">>, _, _) ->
	mk_rep(501, "Illegal PORT command");

handle_command(<<"EPSV">>, _, Args) ->
	ftpd_data_conn:reinit_data_conn(Args),
	{PasvPid, {ok, Port}} = ftpd_data_conn:start_passive_mode(inet6),
	RspStr = "Entering Extended Passive Mode (|||"++integer_to_list(Port)++"|)",
	NewArgs = Args#ctrl_conn_data{ data_pid = PasvPid },
	mk_rep(229, RspStr, NewArgs);

%% format : EPRT<space><d><net-prt><d><net-addr><d><tcp-port><d>
handle_command(<<"EPRT">>, [BinArg1], Args) ->
	Params = binary_to_list(BinArg1),
	IpPortParams = string:tokens(Params,"|"),
	case ?UTIL:eprtlist2portip(IpPortParams) of
		{error, _}   -> mk_rep(500, "EPRT command failed (2)");
		{Addr, Port} ->
			ftpd_data_conn:reinit_data_conn(Args),
			case ftpd_data_conn:start_active_mode(inet6, Addr, Port) of
				{ok, PasvPid} ->
					NewArgs = Args#ctrl_conn_data{ data_pid = PasvPid },
					mk_rep(200, "EPRT command successful", NewArgs);
				{error, _} -> mk_rep(500, "EPRT command failed (1)")
			end
	end;

handle_command(<<"EPRT">>, _, _) ->
	mk_rep(501, "Illegal EPRT command");

handle_command(<<"LIST">>, ParamsBin, Args) ->
	DirToList = ?UTIL:binlist_to_string(ParamsBin),
	AbsPath   = Args#ctrl_conn_data.chrootdir,
	RelPath   = Args#ctrl_conn_data.curr_path,
	case ftpd_dir:set_cwd(AbsPath, RelPath, DirToList) of
		{ok, NewPath} ->
			?UTIL:tracef(Args, ?LIST, [NewPath]),
			FullPath    = AbsPath ++ NewPath,
			io:format("LIST path: ~p\n", [FullPath]),
			{ok, Files} = file:list_dir(FullPath),
			ftpd_data_conn:send_msg(list,{lists:sort(Files),FullPath,lst},Args);
		{error, _} ->
			mk_rep(450, DirToList ++ ": No such file or directory")
	end;

handle_command(<<"NLST">>, ParamsBin, Args) ->
	DirToList = ?UTIL:binlist_to_string(ParamsBin),
	AbsPath   = Args#ctrl_conn_data.chrootdir,
	RelPath   = Args#ctrl_conn_data.curr_path,
	case ftpd_dir:set_cwd(AbsPath, RelPath, DirToList) of
		{ok, NewPath} ->
			io:format("NLST path ~p\n", [NewPath]),
			{ok, Files} = file:list_dir(AbsPath ++ NewPath),
			ftpd_data_conn:send_msg(list, {lists:sort(Files), "", nlst}, Args);
		{error, _} ->
			mk_rep(450, DirToList ++ ": No such file or directory")
	end;


handle_command(<<"REIN">>, [], Args) ->
	NewArgs = Args#ctrl_conn_data{ authed = false, username = none },
	mk_rep(200, "REIN command successful", NewArgs);

handle_command(<<"MKD">>, ParamsBin, Args) ->
	Dir      = ?UTIL:binlist_to_string(ParamsBin),
	RelPath  = Args#ctrl_conn_data.curr_path ++ Dir,
	FullPath = ?UTIL:get_full_path(Args),
	case file:make_dir(FullPath) of
		ok              -> mk_rep(257, "\""++RelPath++"\" directory created");
		{error, eexist} -> mk_rep(550, "Folder already exists");
		{error, _}      -> mk_rep(550, "MKD command failed")
	end;

handle_command(<<"RMD">>, ParamsBin, Args) ->
	Dir      = ?UTIL:binlist_to_string(ParamsBin),
	FullPath = ?UTIL:get_full_path(Args) ++ Dir ++ "/",
	case file:del_dir(FullPath) of
		ok         -> mk_rep(250, "Folder deleted");
		{error, _} -> mk_rep(550, "RMD command failed")
	end;

handle_command(<<"DELE">>, ParamsBin, Args) ->
	Dir      = ?UTIL:binlist_to_string(ParamsBin),
	FullPath = ?UTIL:get_full_path(Args) ++ Dir,
	case file:delete(FullPath) of
		ok         -> mk_rep(250, "File deleted");
		{error, _} -> mk_rep(550, "DELE command failed")
	end;

handle_command(<<"RNFR">>, ParamsBin, Args) ->
	FromName = ?UTIL:binlist_to_string(ParamsBin),
	FullPath = ?UTIL:get_full_path(Args) ++ FromName,
	NewArgs  = Args#ctrl_conn_data{ rename_from = FullPath },
	mk_rep(350, "Requested file action pending further information.", NewArgs);

handle_command(<<"RNTO">>, ParamsBin, Args) ->
	ToName = ?UTIL:binlist_to_string(ParamsBin),
	ToPath = ?UTIL:get_full_path(Args) ++ ToName,
	case Args#ctrl_conn_data.rename_from of
		none ->
			mk_rep(550, "RNTO command failed (1)");
		FromPath ->
			io:format("From: ~p || To: ~p", [FromPath, ToPath]),
			NewArgs = Args#ctrl_conn_data{ rename_from = none },
			case file:rename(FromPath, ToPath) of
				ok -> mk_rep(250, "RNTO ok", NewArgs);
				_  -> mk_rep(550, "RNTO command failed (2)", NewArgs)
			end
	end;

handle_command(<<"">>, _, _) ->
	mk_rep(500, "Invalid command: try being more creative");

handle_command(Command, _, _) ->
	mk_rep(500, binary_to_list(Command) ++ " not implemented").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Control functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mk_rep(Code, Message) ->
	{?RESP(Code, Message), sameargs}.
mk_rep(Code, Message, NewArgs) ->
	{?RESP(Code, Message), {newargs, NewArgs}}.

%% Send response if needed
handle_reply(_, noreply) ->
	ok;
handle_reply(Sock, {reply, Code, Message}) ->
	?UTIL:send_reply(Sock, Code, Message).

%% Control behaviour after replying to a message
after_reply(Sock, <<"QUIT">>, _) ->
	io:format("---------------- CONNECTION CLOSE ----------------\n"),
	gen_tcp:close(Sock);
after_reply(Sock, _, Args) ->
	do_recv(Sock, Args).
