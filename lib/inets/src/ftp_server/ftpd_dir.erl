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

-export([short_list_dir/3, long_list_dir/3, canonicalize_path/1]).

% used in NLST, Cwd is full path!
short_list_dir(RootDir, Cwd, Path) ->
    case filename:pathtype(Path) of
	absolute -> short_list_absolute_dir(RootDir, Path);
	relative -> short_list_relative_dir(RootDir, Cwd, Path)
    end.

short_list_absolute_dir(RootDir, LPath) ->
    % Needs to canonicalize the path, otherwise .. would go out of the chroot
    LocalPath = canonicalize_path(LPath),
    case file:list_dir(RootDir++LocalPath) of
	{ok, FileNames} ->
	    {ok, [filename:join(LocalPath, FileName) || FileName <- lists:sort(FileNames)]};
	{error, enotdir} ->
	    % Path is a file
	    {ok, [LocalPath]};
	_Error ->
	    % Intentional: both the Solaris and Linux servers seems to
	    % return empty data on error instead of error codes
	    {ok, []}
    end.

short_list_relative_dir(RootDir, Cwd, CPath) ->
    % Needs to canonicalize the path, otherwise .. would go out of the chroot
    LocalPath = canonicalize_path(filename:join(Cwd, CPath)),
    case file:list_dir(RootDir++LocalPath) of
	{ok, FileNames} ->
	    case CPath of
		"" ->
		    {ok, lists:sort(FileNames)};
		_ ->
		    {ok, [filename:join(CPath, FileName) || FileName <- lists:sort(FileNames)]}
	    end;
	{error, enotdir} ->
	    % Path is a file
	    {ok, [CPath]};
	_Error ->
	    % Intentional: both the Solaris and Linux servers seems to
	    % return empty data on error instead of error codes
	    {ok, []}
    end.

long_list_dir(RootDir, Cwd, Path) ->
    LocalPath = RootDir++filename:join(Cwd, Path),
    case file:list_dir(LocalPath) of
	{ok, FileNames} ->
	    {ok, [ftpd_util:get_file_info(filename:join(LocalPath,FileName)) || FileName <- lists:sort(FileNames)]};
	{error, enotdir} ->
	    % Path is a file
	    {ok, [ftpd_util:get_file_info(LocalPath)]};
	_Error ->
	    % Intentional: both the Solaris and Linux servers seems to
	    % return empty data on error instead of error codes
	    {ok, []}
    end.

canonicalize_path(Path) ->
    PathElements = filename:split(Path),
    canonicalize_path(PathElements, [""]).

canonicalize_path([], ResultList) ->
    filename:join(lists:reverse(ResultList));
canonicalize_path(["." | Rest], ResultList) ->
    canonicalize_path(Rest, ResultList);
% don't crash when somebody wants to go upper than the root
canonicalize_path([".." | Rest], ["/" | _ ]) ->
    canonicalize_path(Rest, ["/"]);
canonicalize_path([".." | Rest], [_Parent | ResultList]) ->
    canonicalize_path(Rest, ResultList);
canonicalize_path([Dir | Rest], ResultList) ->
    canonicalize_path(Rest, [Dir | ResultList]).


