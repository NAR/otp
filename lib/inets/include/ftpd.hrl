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

% Public macros for the log and trace functions

% events to log
-define(LOGIN_OK, login_ok).
-define(LOGIN_FAIL, login_failed).
-define(CONN_CLOSE, connection_closed).

% events to trace
-define(CWD, <<"CWD">>).
-define(RETR, <<"RETR">>).
-define(STOR, <<"STOR">>).
-define(LIST, <<"LIST">>).

