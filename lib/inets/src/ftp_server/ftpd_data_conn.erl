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

-module(ftpd_data_conn).

-export([start_passive_mode/1, start_active_mode/3,
	 pasv_accept/1, actv_accept/1,
	 reinit_data_conn/1, send_msg/3]).

-include_lib("inets/include/ftpd.hrl").
-include_lib("ftpd_rep.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Passive mode
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_active_mode(Ipv, Addr, Port) ->
    ?LOG("Active mode begin, port: ~p\n", [Port]),
    SockArgs = [binary, {packet, 0}, {active, false}] ++ ipv_argl(Ipv),
    case gen_tcp:connect(Addr, Port, SockArgs) of
	{ok, Sock} -> {ok, spawn(?MODULE, actv_accept, [Sock])};
	Error      -> Error
    end.

start_passive_mode(Ipv) ->
    ?LOG("Passive mode begin\n"),
    SockArgs    = [binary, {packet, 0}, {active, false}] ++ ipv_argl(Ipv),
    case gen_tcp:listen(0, SockArgs) of
	{ok, LSock} -> 
	    Pid = spawn(?MODULE, pasv_accept, [LSock]),
	    case inet:port(LSock) of
		{ok, Port} ->
		    ?LOG("[~p]: Passive mode started, port: ~p\n", [Pid, Port]),
		    {ok, {Pid, Port}};
		Error -> Error
	    end;
	Error -> Error
    end.

ipv_argl(inet4) -> [];
ipv_argl(inet6) -> [inet6].

reinit_data_conn(Args) ->
    case Args#ctrl_conn_data.data_pid of
	none    -> true;
	LastPid -> exit(LastPid, kill)
    end.

send_msg(MsgType, Msg, Args) ->
    case Args#ctrl_conn_data.data_pid of
	none ->
	    send_ctrl_reply(Args, 500, "Data connection not established.");
	DataPid ->
	    send_ctrl_reply(Args, 150, "Opening BINARY mode data connection"),
	    DataPid ! {MsgType, Msg, Args}
    end,
    {noreply, sameargs}.

actv_accept(DataSock) ->
    ?LOG("ACTV accept start\n"),
    data_conn_main(DataSock).

pasv_accept(LSock) ->
    ?LOG("PASV accept start\n"),
    case gen_tcp:accept(LSock) of
	{ok, Sock} -> data_conn_main(Sock);
	_	  -> err_tcp
    end.

data_conn_main(DataSock) ->
    ?LOG("~p PASV send loop\n", [DataSock]),
    receive
	{list, FileNames, Args} ->
	    TempMsg = [FN ++ "\r\n"|| FN <- FileNames],
	    FormattedMsg = lists:flatten(TempMsg),
	    ?UTIL:send_message(DataSock, FormattedMsg),
	    transfer_complete(Args);
	{retr, FileName, Args} ->
	    AbsPath = Args#ctrl_conn_data.chrootdir,
	    RelPath = Args#ctrl_conn_data.curr_path,
	    FPath   = ftpd_dir:canonicalize_path(AbsPath ++ filename:join(RelPath,FileName)),
	    case file:read_file(FPath) of
		{ok, Bin} ->
		    BinT = ?UTIL:transformto(Bin,Args#ctrl_conn_data.repr_type),
		    ?UTIL:send_message(DataSock, BinT),
		    TraceParams = [RelPath ++ "/" ++ FileName, FileName],
		    ?UTIL:tracef(Args, ?RETR, TraceParams), %% TODO 2nd param ??
		    transfer_complete(Args);
		{error, _} ->
		    RespStr = "Requested action not taken. File unavailable, "
			      "not found, not accessible",
		    send_ctrl_reply(Args, 550, RespStr)
	    end;
	{stor, {FileName, Mode}, Args} ->
	    AbsPath = Args#ctrl_conn_data.chrootdir,
	    RelPath = Args#ctrl_conn_data.curr_path,
	    FPath   = ftpd_dir:canonicalize_path(AbsPath ++ filename:join(RelPath,FileName)),
	    Repr    = Args#ctrl_conn_data.repr_type,
	    case receive_and_store(DataSock, FPath, Mode, Repr) of
		ok ->
		    TraceParams = [filename:join(RelPath, FileName)],
		    ?UTIL:tracef(Args, ?STOR, TraceParams),
		    transfer_complete(Args);
		{error, _} ->
		    RespStr = "Requested action not taken. File unavailable, "
			      "not found, not accessible",
		    send_ctrl_reply(Args, 550, RespStr)
	    end
    end,
    ?LOG("PASV send loop end\n"),
    gen_tcp:close(DataSock).

%% Receive binaries and store them in a file
receive_and_store(DataSock, FPath, Mode, ReprType) ->
    case file:open(FPath, [Mode, binary]) of
	{ok, Id} ->
	    Receive = receive_and_write_chunks(DataSock, Id, ReprType),
	    Close   = file:close(Id),
	    case {Receive, Close} of
		{ok, ok} -> ok;
		_	-> {error, receive_fail}
	    end;
	Error -> Error
    end.

receive_and_write_chunks(DataSock, DevId, ReprType) ->
    case gen_tcp:recv(DataSock, 0) of
	{ok, Data} ->
	    file:write(DevId, ?UTIL:transformfrom(Data, ReprType)),
	    receive_and_write_chunks(DataSock, DevId, ReprType);
	{error, closed} -> ok;
	{error, Reason} -> {error, Reason}
    end.

transfer_complete(Args) ->
    send_ctrl_reply(Args, 226, "Transfer complete").

send_ctrl_reply(Args, Command, Msg) ->
    case Args#ctrl_conn_data.control_socket of
	none -> ?LOG("Error: control connection lookup failed");
	Sock -> ?UTIL:send_reply(Sock, Command, Msg)
    end.
