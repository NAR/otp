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

-module(ftpd_dir).

-export([list_dir/1,canonicalize_path/1]).

list_dir(DirName) ->
    Path = canonicalize_path(DirName),
    case file:list_dir(Path) of
	{ok, FileNames} ->
	    {ok, Path, FileNames};
	{error, enotdir} ->
	    % Path is a file
	    {ok, Path, [filename:basename(Path)]};
	_Error ->
	    % Intentional: both the Solaris and Linux servers seems to
	    % return empty data on error instead of error codes
	    {ok, Path, []}
    end.

canonicalize_path(Path) ->
    PathElements = filename:split(Path),
    canonicalize_path(PathElements, "").

canonicalize_path([], ResultList) ->
    filename:join(lists:reverse(ResultList));
canonicalize_path(["." | Rest], ResultList) ->
    canonicalize_path(Rest, ResultList);
% don't crash when somebody wants to go upper than the root
canonicalize_path([".." | Rest], ["/"]) ->
    canonicalize_path(Rest, ["/"]);
canonicalize_path([".." | Rest], [_Parent | ResultList]) ->
    canonicalize_path(Rest, ResultList);
canonicalize_path([Dir | Rest], ResultList) ->
    canonicalize_path(Rest, [Dir | ResultList]).

