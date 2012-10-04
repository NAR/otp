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

-module(ftpd_sup).
-behaviour(supervisor).

-export([start_link/0, start_link/1, start_link/2,
        start_child/1, stop_child/1]).

-export([init/1,stop/1]).

-define(SUP_SPEC, {one_for_one, 1, 60}).
-include_lib("ftpd_rep.hrl").

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_link(Config) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Config).

start_link(Config, stand_alone) ->
    supervisor:start_link(?MODULE, Config).

stop(Pid) ->
	exit(Pid,shutdown), %% TODO error reporting
	ok.

init([]) ->
	{ok, {?SUP_SPEC, []}};

init(Config) ->
	ChildSpec = get_listener_child_spec(self(), Config),
	{ok, {?SUP_SPEC, [ChildSpec]}}.

start_child(Config) ->
	ChildSpec = get_listener_child_spec(self(), Config),
	supervisor:start_child(?MODULE, ChildSpec).

stop_child(Pid) ->
	case get_child_id(Pid) of
		{error, R} -> {error, R};
		Id ->
    		case supervisor:terminate_child(?MODULE, Id) of
        		ok ->
           			supervisor:delete_child(?MODULE, Id);
        		Error ->
            		Error
			end
    end.


set_child_id(Config) ->
    Address = proplists:get_value(bind_address, Config, no_address),
    Port = proplists:get_value(port, Config, ?DEFAULT_PORT),
    Fd = proplists:get_value(fd, Config, no_fd),
    {listener_srv, {Address, Port, Fd}}.


get_child_id(Pid) ->
	case lists:keyfind(Pid, 2, supervisor:which_children(?MODULE)) of
		{Id, Pid, _, _} -> Id;
		_ -> {error, child_not_found}
	end.

get_listener_child_spec(SupPid, Config) ->
	NewArgs = [ {sup_pid, SupPid} | Config],
	ChildId = set_child_id(Config),
	{ChildId, {ftpd_listener, start_link, [NewArgs]},
       		     permanent, 100000, worker, [ftpd_listener]}.

