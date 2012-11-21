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

-module(ftpd_util).

-export([format_address/2, packet_to_tokens/1, check_repr_type/1,
         response/2, send_reply/3, send_message/2,
         check_auth/2, implemented_msgs/0,
         get_file_info/2, get_full_path/1, concat_paths/2,
         transformfrom/2, transformto/2,
         logf/3, tracef/3,
         list2portip/1, eprtlist2portip/1, get_server_ip/0, getaddr/1,
         bin_to_upper/1, binlist_to_string/1]).

-include_lib("ftpd_rep.hrl").
-include_lib("kernel/include/inet.hrl").

%% Converts ip and port to "h1,h2,h3,h4,p1,p2" format
format_address({A1, A2, A3, A4}, Port) ->
	<<P1:8, P2:8>> = <<Port:16>>,
	lists:concat([A1,",",A2,",",A3,",",A4,",",P1,",",P2]).

bin_to_upper(T) ->
	<< <<if (X=<$z)and(X>=$a) -> X-($a-$A); true -> X end>> || <<X:8>> <= T >>.

binlist_to_string(List) ->
	StrList = [ binary_to_list(E) || E <- List],
	string:join(StrList, " ").

%% Separate command from message and convert to upper case
%% eg. "user someone" -> {<<"USER">>, [<<"someone">>]}
-spec packet_to_tokens(Data :: bitstring()) ->
	{Command :: bitstring(), [Message :: bitstring()]}.
packet_to_tokens(Data) ->
	TrimmedData  = re:replace(Data, "\r\n", "",[{return,list}]),
	SplittedData = re:split(TrimmedData, " "),
	case SplittedData of
		[Command | Msg] -> {bin_to_upper(Command), Msg};
		_               -> ?LOG("Error: packet parse failed\n"),
                           {<<"">>, []}
	end.

%% check for TYPE command arguments
check_repr_type([Type])     -> lists:member(Type, ["I","A"]);
check_repr_type(["L", Arg]) -> Arg == "8";
check_repr_type(_)          -> false.

%% All implemented commands
implemented_msgs() ->
	[<<"NOOP">>, <<"QUIT">>, <<"USER">>, <<"PASS">>, <<"TYPE">>,
	 <<"SIZE">>, <<"RETR">>, <<"STOR">>, <<"APPE">>, <<"CWD">>,
	 <<"PWD">>,  <<"STRU">>, <<"PASV">>, <<"PORT">>, <<"EPSV">>,
	 <<"EPRT">>, <<"LIST">>, <<"NLST">>, <<"REIN">>, <<"MKD">>,
	 <<"RMD">>,  <<"DELE">>, <<"RNFR">>, <<"RNTO">>].

%% Messages that does not require USER and PASS before
no_auth_msgs() ->
	[<<"USER">>, <<"PASS">>, <<"QUIT">>, <<"NOOP">>, <<"ACCT">>,
	 <<"TYPE">>, <<"FEAT">>].

check_auth(Command, Args) ->
	Impl   = lists:member(Command, implemented_msgs()),
	NoAuth = lists:member(Command, no_auth_msgs()),
	Authed = Args#ctrl_conn_data.authed,
	case {Impl, NoAuth, Authed} of
		{false, _,    _    } -> ok;
		{true, false, false} -> bad;
		_                    -> ok
	end.

%% Construct tuple for response
-spec response(ReplyCode :: integer(), Message :: string()) -> reply().
response(ReplyCode, Message) -> {reply, ReplyCode, Message}.

%% Convert Code and Message to packet and send
send_reply(Sock, Code, Message) ->
	Str = integer_to_list(Code) ++ " " ++ Message ++ "\r\n",
	send_message(Sock, Str).

%% Send raw message
send_message(Sock, Str) ->
	?LOG("[~p-Send]: ~p\n", [Sock, Str]),
	gen_tcp:send(Sock, Str).

%% Get file information
%% drwxrwsr-x   3 47688    60000        4096 Dec-9-2005 empty
get_file_info(FName, FullPath) ->
	{ok, {file_info, Size, Type, _Access,
	_AccTime, {{MY,MM,MD}, {MH,MMin,_MS}},
	_CreTime,
    Mode, Links,
	_MajorDev, _MinorDev, _INode, UID, GID}}
		= file:read_file_info(concat_paths(FullPath, FName)),

	Time = lists:concat(case MY < current_year() of
		true  -> [MY];
		false -> [MH,":",MMin]
	end),

	lists:concat([get_type_letter(Type),get_modes(Mode),
	" ",Links," ",UID," ",GID," ",Size," ",
	httpd_util:month(MM)," ",MD," ",Time," ",FName]).

get_type_letter(device)    -> "b";
get_type_letter(directory) -> "d";
get_type_letter(regular)   -> "-";
get_type_letter(symlink)   -> "l";
get_type_letter(other)     -> "-";
get_type_letter(_)         -> "-".

current_year() ->
	{{Year,_,_}, _} = calendar:local_time(),
	Year.

get_modes(Mode) ->
	lists:concat([get_rights(Mode, 1 bsl 6), get_rights(Mode, 1 bsl 3), get_rights(Mode, 1)]).

get_rights(Mode, User) ->
	lists:concat([check_band(Mode, User bsl 2, "r"), check_band(Mode, User bsl 1, "w"), check_band(Mode, User, "x")]).

check_band(A, B, R) ->
	case A band B of
		0 -> "-";
		_ -> R
	end.

get_full_path(Args) ->
	AbsPath = Args#ctrl_conn_data.chrootdir,
	RelPath = Args#ctrl_conn_data.curr_path,
	AbsPath ++ RelPath.

concat_paths(P1, P2) ->
	Temp = P1 ++ "/" ++ P2,
	re:replace(Temp, "\/+", "\/", [{return, list}, global]).

%% CRLF transformation from ASCII to own representation
transformfrom(Bin, _) ->
	Bin.

%% CRLF transformation from own representation to ASCII
transformto(Bin, ["A"]) ->
	Step1 = re:replace(Bin, "\r\n", "\n", [{return, binary}, global]),
	re:replace(Step1, "\n", "\r\n", [{return, binary}, global]);
transformto(Bin, _) ->
	Bin.

%% Log and trace functions
logf(ConnData, Event, Params) ->
	LogFun = ConnData#ctrl_conn_data.log_fun,
	LogFun(Event, Params).
tracef(ConnData, Event, Params) ->
	TraceFun = ConnData#ctrl_conn_data.trace_fun,
	TraceFun(Event, Params).

%% Conversion between string list and IP/Port tuple
list2portip(Lst) when length(Lst) == 6 ->
	Fun = fun(A) -> {Res, _} = string:to_integer(A), Res end,
	[A1,A2,A3,A4,P1,P2] = [ Fun(X) || X <- Lst ],
	case lists:member(error,[A1,A2,A3,A4,P1,P2]) of
		false ->
			<<Port:16>> = <<P1:8, P2:8>>,
			{ok, {{A1,A2,A3,A4}, Port}};
		true ->
			{error, bad_addr}
	end;
list2portip(_) ->
	{error, bad_addr}.

eprtlist2portip([Tp, SAddr, SPort]) when ((Tp == "1") or (Tp == "2")) ->
	case {inet_parse:address(SAddr), string:to_integer(SPort)} of
		{{ok, IP}, {Port, []}} -> {ok, {IP, Port}};
		_Error                 -> {error, bad_addr}
	end;

eprtlist2portip(_) ->
	{error, bad_addr}.

get_server_ip() ->
	{ok, Name} = inet:gethostname(),
	case inet_res:gethostbyname(Name) of
		{ok, HostInfo} -> {ok, hd(HostInfo#hostent.h_addr_list)};
		{error, _}     -> inet:getaddr(Name, inet)
	end.

getaddr(Addr) ->
	case inet:getaddr(Addr,inet) of
		{error, _} -> inet:getaddr(Addr,inet6);
		Res 	   -> Res
	end.
