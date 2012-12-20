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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% FTPD inner representation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-ifndef(ftpd_rep_hrl).
-define(ftpd_rep_hrl, true).

%% Defines

%-define(LOG_MODE, true).

-define(UTIL, ftpd_util).
-define(RESP(Comm, Msg), ?UTIL:response(Comm, Msg)).

-define(DEFAULT_PORT,     6221).
-define(DEFAULT_ROOT_DIR, element(2, file:get_cwd())).
-define(DEFAULT_PWD_FUN,  fun(_,_) -> not_authorized end).
-define(DEFAULT_LOG_FUN,  fun(_,_) -> ok end).

-ifdef(LOG_MODE).
 -define(LOG(Str, Args), io:format(Str, Args)).
 -define(LOG(Str), io:format(Str)).
-else.
 -define(LOG(Str, Args), ok).
 -define(LOG(Str), ok).
-endif.

-define(is_anon(Args), ((Args#ctrl_conn_data.username == "anonymous") or
			(Args#ctrl_conn_data.username == "ftp")) and
			(Args#ctrl_conn_data.allow_anonymous)).

%% Control connection data
%%  username     ~ actual username set by the user (using USER <loginname>)
%%  authed       ~ indicates whether a proper password
%%		 was given for the actual username
%%  control_sock ~ socket of the control connection
%%  pasv_pid     ~ PID of the passive connection used by this control connection
%%		 (if exists)
%%  curr_path    ~ current directory path

-record(ctrl_conn_data, {
			chrootdir	= none,
			pwd_fun		= ?DEFAULT_PWD_FUN,
			log_fun		= ?DEFAULT_LOG_FUN,
			trace_fun	= ?DEFAULT_LOG_FUN,
			allow_anonymous = false,

			session_state	= none,
			control_socket	= none,

			data_pid	= none,
			username	= none,
			authed		= false,
			curr_path	= "/",
			repr_type	= ["I"],
			rename_from	= none
			}).

%% Types

-type proplist()   :: proplists:proplist().
-type socket()     :: gen_tcp:socket().

-type reply()      :: {reply, ReplyCode :: integer(), Message :: string()}.
-type argschange() :: sameargs | {newargs, NewArgs :: proplist()}.

-endif.
