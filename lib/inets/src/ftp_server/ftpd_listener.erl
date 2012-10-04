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

-module(ftpd_listener).
-behaviour(gen_server).

-export([start_link/1, loop/2, info/1]).
-export([init/1, handle_call/3, handle_cast/2,
         terminate/2, code_change/3, handle_info/2]).

-include_lib("ftpd_rep.hrl").

loop(LSock, Args) ->
	case gen_tcp:accept(LSock) of
		{ok, Sock} ->
			spawn(ftpd_ctrl_conn, new_connection, [Sock, Args]),
			loop(LSock, Args);
		{_, _Res} -> err_tcp
	end.

start_link(Args) ->
    Address = proplists:get_value(bind_address, Args, no_address),
    Port    = proplists:get_value(port, Args, ?DEFAULT_PORT),
    Fd 	    = proplists:get_value(fd, Args, no_fd),
    Str     = lists:flatten(io_lib:format("ftpd_listener_~p:~p,~p", [Address, Port, Fd])),
    RegName = list_to_atom(Str),
    gen_server:start_link({local, RegName}, ?MODULE, Args, []).

init(Args) ->
	process_flag(trap_exit, true),

	Port     = proplists:get_value(port, Args, ?DEFAULT_PORT),
	BaseArgs = [binary, {packet, 0}, {active, false}],

	Args1 = BaseArgs ++
		case proplists:lookup(bind_address, Args) of
			{bind_address, Addr={_,_,_,_,_,_,_,_}} -> [{ip, Addr}, inet6];
			{bind_address, Addr={_,_,_,_}}         -> [{ip, Addr}];
			{bind_address, Addr}                   ->
				case ?UTIL:getaddr(Addr) of
					{ok, IP} -> [{ip, IP}];
					_		 -> []
				end;
			none                                   -> []
		end,
	Args2 = Args1 ++
		case proplists:lookup(fd, Args) of
			none   -> [];
			FdProp -> [FdProp]
		end,
	case gen_tcp:listen(Port, Args2) of
		{ok, LSock} ->
    		spawn(?MODULE, loop, [LSock, Args]),
			{ok, {Port, Args, LSock}};
		Error -> Error
	end.

info(Pid) ->
	gen_server:call(Pid, info).

handle_call(info, _From, {Port, Args, LSock}) ->
	NewArgs =
		case proplists:lookup(bind_address, Args) of
			none -> [{bind_address, localhost} | Args];
			_    -> Args
		end,
	Reply =
		case proplists:lookup(port, NewArgs) of
			none -> [{port, Port} | NewArgs];
			_    -> NewArgs
		end,
	{reply, Reply, {Port, Args, LSock}};
handle_call(info, _From, State) -> {noreply, State}.

handle_cast(_Req, State) -> {noreply, State}.

%terminate(shutdown, State) removed, same body
%terminate({shutdown, _Reason}, State)
terminate(_Reason, State) ->
	?LOG("Listener terminated\n"),
	LSock = element(3, State),
	gen_tcp:close(LSock),
	ok.

handle_info(_Info, _State) ->
	{stop, normal}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
