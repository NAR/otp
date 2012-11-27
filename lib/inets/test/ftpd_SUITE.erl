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

-module(ftpd_SUITE).

-include_lib("test_server/include/test_server.hrl").
-include("test_server_line.hrl").
-include_lib("inets/include/ftpd.hrl").

%% Test server specific exports
-export([all/0, suite/0, groups/0, init_per_group/2, end_per_group/2]).
-export([init_per_testcase/2, end_per_testcase/2,
	 init_per_suite/1, end_per_suite/1]).

-export([start_stop_test/1,
	 connect_test/1,
	 multiple_servers_test/1,
	 connect_v6_test/1,
     	 login_success_test/1,
     	 login_failure_test/1,
         info_test/1,
	 ls_test/1,
	 ls_dir_test/1,
	 ls_empty_dir_test/1,
	 nlist_test/1,
	 nlist_file_test/1,
	 cd_test/1,
	 pwd_test/1,
	 cd_ls_test/1,
	 cd_nlist_test/1,
	 download_test/1,
	 upload_test/1,
	 mkdir_test/1,
	 fd_test/1,
	 log_trace_test/1,
	 chunk_test/1,
	 split_command_test/1,
         cd_up_from_root_test/1,
         cd_over_symlink_test/1,
         unreadable_dir_test/1,
         unreadable_file_test/1,
         unwritable_dir_test/1,
         unwritable_file_test/1,
         read_dir_test/1,
         write_dir_test/1
	]).

-define(USER, "test").
-define(PASS, "test").
-define(WRONGPASS, "test2").

%%--------------------------------------------------------------------
%% all(Arg) -> [Doc] | [Case] | {skip, Comment}
%% Arg - doc | suite
%% Doc - string()
%% Case - atom()
%%	Name of a test case function.
%% Comment - string()
%% Description: Returns documentation/test cases in this test suite
%%		or a skip tuple if the platform is not supported.
%%--------------------------------------------------------------------
suite() -> [{timetrap, {seconds, 30}}].

all() -> [
	{group, basic_tests},
	{group, login_tests},
	{group, directory_tests},
    	{group, download_upload_tests},
	{group, ipv6_tests},
	{group, log_trace_tests},
	{group, negative_tests}
    ].

groups() ->
    [{basic_tests, [], [start_stop_test, connect_test, multiple_servers_test, connect_v6_test, fd_test]},
     {login_tests, [], [login_success_test, login_failure_test, info_test]},
     {directory_tests, [parallel], [ls_test, ls_dir_test, ls_empty_dir_test, 
	     nlist_test, nlist_file_test, cd_test, pwd_test, cd_ls_test, cd_nlist_test]},
     {download_upload_tests, [], [download_test, upload_test, mkdir_test, chunk_test]},
     {ipv6_tests, [], [ls_test, ls_dir_test, ls_empty_dir_test, cd_test, download_test, upload_test]},
     {log_trace_tests, [], [log_trace_test]},
     {negative_tests, [], [split_command_test, cd_up_from_root_test, cd_over_symlink_test,
         unreadable_dir_test, unreadable_file_test, unwritable_dir_test, unwritable_file_test,
         read_dir_test, write_dir_test]}
    ].

init_per_suite(Config) ->
    DataDir = ?config(data_dir, Config),
    file:make_dir(filename:join([DataDir, "empty_dir"])),
    ok = inets:start(),
    Config.

end_per_suite(Config) ->
    inets:stop(),
    Config.

init_per_group(basic_tests, Config) ->
    Config;
init_per_group(log_trace_tests, Config) ->
    TableOwner = spawn(fun() -> receive test_finished -> ok end end),
    Tid = ets:new(logtrace, [public, named_table, {heir, TableOwner, none}]),
    DataDir = ?config(data_dir, Config),
    {ok, Pid} = inets:start(ftpd, [{port, 2021},
	    			   {pwd_fun, fun pwdfun/2},
				   {chrootDir, DataDir},
			       	   {log_fun, fun logfun/2},
			       	   {trace_fun, fun tracefun/2}]),
    [{ftpd_pid, Pid}, {ftp_server_address, "localhost"},
     {table_owner, TableOwner}, {table, Tid} | Config];

init_per_group(ipv6_tests, Config) ->
    DataDir = ?config(data_dir, Config),
    FtpHost = {0,0,0,0,0,0,0,1},
    {ok, Pid} = inets:start(ftpd, [{bind_address, FtpHost},
	    			   {port, 2021},
				   {pwd_fun, fun pwdfun/2},
				   {chrootDir, DataDir}]),
			   [{ftpd_pid, Pid}, {ftp_server_address, FtpHost} | Config];

init_per_group(_Group, Config) ->
    DataDir = ?config(data_dir, Config),
    {ok, Pid} = inets:start(ftpd, [{port, 2021}, {pwd_fun, fun pwdfun/2}, {chrootDir, DataDir}]),
    [{ftpd_pid, Pid}, {ftp_server_address, "localhost"} | Config].

end_per_group(basic_tests, Config) ->
    Config;

end_per_group(log_trace_tests, Config) ->
    ets:delete(?config(table, Config)),
    ?config(table_owner, Config) ! test_finished,
    Pid = ?config(ftpd_pid, Config),
    inets:stop(ftpd, Pid);

end_per_group(_Group, Config) ->
    Pid = ?config(ftpd_pid, Config),
    inets:stop(ftpd, Pid).

init_per_testcase(start_stop_test, Config) ->
    Config;
init_per_testcase(connect_test, Config) ->
    Config;
init_per_testcase(multiple_servers_test, Config) ->
    Config;
init_per_testcase(connect_v6_test, Config) ->
    Config;
init_per_testcase(fd_test, Config) ->
    Config;

init_per_testcase(upload_test, Config0) ->
    Config = ftp_connect(Config0),
    PrivDir = ?config(priv_dir, Config),
    EmptyFileName = "empty_upload",
    DataFileName = "data_upload",
    ok = file:write_file(filename:join(PrivDir, EmptyFileName), <<>>),
    ok = file:write_file(filename:join(PrivDir, DataFileName), <<"ABC">>),
    [{empty_file_name, EmptyFileName}, {data_file_name, DataFileName}| Config];

init_per_testcase(mkdir_test, Config0) ->
    Config = ftp_connect(Config0),
    PrivDir = ?config(priv_dir, Config),
    RemoteDir = "remote_dir",
    DataFileName = "data_upload",
    ok = file:write_file(filename:join(PrivDir, DataFileName), <<"ABC">>),
    [{remote_dir, RemoteDir}, {data_file_name, DataFileName}| Config];

init_per_testcase(split_command_test, Config0) ->
    {ok, Sock} = gen_tcp:connect("localhost", 2021, [list, {active, false}, {nodelay, true}]),
    [{sock, Sock} | Config0];

init_per_testcase(cd_over_symlink_test, Config) ->
    DataDir = ?config(data_dir, Config),
    OutLink = filename:join([DataDir, "tmp"]),
    ok = file:make_symlink("/tmp", OutLink),
    [{out_link, OutLink} | ftp_connect(Config)];

init_per_testcase(unreadable_dir_test, Config) ->
    DataDir = ?config(data_dir, Config),
    UnreadDir = filename:join([DataDir, "tmp"]),
    ok = file:make_dir(UnreadDir),
    % actually the execution bit is important, but nevertheless, remove all rights
    ok = file:change_mode(UnreadDir, 0),
    [{un_name, UnreadDir} | ftp_connect(Config)];

init_per_testcase(unreadable_file_test, Config) ->
    DataDir = ?config(data_dir, Config),
    UnreadFile = filename:join([DataDir, "tmp"]),
    ok = file:write_file(UnreadFile, <<"abc">>),
    ok = file:change_mode(UnreadFile, 0),
    [{un_name, UnreadFile} | ftp_connect(Config)];

init_per_testcase(unwritable_dir_test, Config) ->
    DataDir = ?config(data_dir, Config),
    UnwriteDir = filename:join([DataDir, "tmp"]),
    ok = file:make_dir(UnwriteDir),
    FileToRename = filename:join([UnwriteDir, "tmp"]),
    ok = file:write_file(FileToRename, <<"abc">>),
    ok = file:change_mode(UnwriteDir, 8#00555),
    [{un_name, UnwriteDir}, {file_to_rename, FileToRename} | ftp_connect(Config)];

init_per_testcase(unwritable_file_test, Config) ->
    DataDir = ?config(data_dir, Config),
    UnwriteFile = filename:join([DataDir, "tmp"]),
    ok = file:write_file(UnwriteFile, <<"abc">>),
    ok = file:change_mode(UnwriteFile, 8#00400),
    [{un_name, UnwriteFile} | ftp_connect(Config)];

init_per_testcase(_Case, Config) ->
    ftp_connect(Config).

end_per_testcase(start_stop_test, Config) ->
    Config;
end_per_testcase(connect_test, Config) ->
    Config;
end_per_testcase(multiple_servers_test, Config) ->
    Config;
end_per_testcase(connect_v6_test, Config) ->
    Config;
end_per_testcase(fd_test, Config) ->
    Config;
end_per_testcase(log_trace_tests, Config) ->
    Config;

end_per_testcase(upload_test, Config) ->
    % Remove the uploaded files
    DataDir = ?config(data_dir, Config),
    EmptyFileName = ?config(empty_file_name, Config),
    DataFileName = ?config(data_file_name, Config),
    file:delete(filename:join(DataDir, EmptyFileName)),
    file:delete(filename:join(DataDir, DataFileName)),
    ftp_close(Config);

end_per_testcase(mkdir_test, Config) ->
    % Remove the uploaded files
    DataDir = ?config(data_dir, Config),
    DataFileName = ?config(data_file_name, Config),
    RemoteDir = ?config(remote_dir, Config),
    file:delete(filename:join([DataDir, RemoteDir, DataFileName])),
    file:del_dir(filename:join([DataDir, RemoteDir])),
    ftp_close(Config);

end_per_testcase(split_command_test, Config) ->
    Sock = ?config(sock, Config),
    gen_tcp:close(Sock);

end_per_testcase(cd_over_symlink_test, Config) ->
    OutLink = ?config(out_link, Config),
    ok = file:delete(OutLink),
    ftp_close(Config);

end_per_testcase(unwritable_dir_test, Config) ->
    UnName = ?config(un_name, Config),
    File = ?config(file_to_rename, Config),
    ok = file:change_mode(UnName, 8#00777),
    ok = file:delete(File),
    ok = file:del_dir(UnName),
    ftp_close(Config);

end_per_testcase(unreadable_dir_test, Config) ->
    UnName = ?config(un_name, Config),
    ok = file:change_mode(UnName, 8#00666),
    ok = file:del_dir(UnName),
    ftp_close(Config);

end_per_testcase(UT, Config) when UT =:= unreadable_file_test; 
                                  UT =:= unwritable_file_test ->
    UnName = ?config(un_name, Config),
    ok = file:change_mode(UnName, 8#00666),
    ok = file:delete(UnName),
    ftp_close(Config);

end_per_testcase(_Case, Config) ->
    ftp_close(Config).

start_stop_test(doc) ->
    ["Test that the FTP server starts at all"];
start_stop_test(suite) ->
    [];
start_stop_test(_Config) ->
    {ok, Pid} = inets:start(ftpd, [{port, 2021}]),
    inets:stop(ftpd, Pid).

connect_test(doc) ->
    ["Test that we can connect to the ftp server"];
connect_test(suite) ->
    [];
connect_test(_Config) ->
    {ok, Pid} = inets:start(ftpd, [{port, 2021}]),
    {ok, Ftp} = ftp:open("localhost", [{port,2021}]),
    ok = ftp:close(Ftp),
    inets:stop(ftpd, Pid).

multiple_servers_test(doc) ->
    ["Test that we can start and connect to multiple ftp servers in the same node"];
multiple_servers_test(suite) ->
    [];
multiple_servers_test(_Config) ->
    {ok, Pid1} = inets:start(ftpd, [{bind_address, {127,0,0,1}}, {port, 2021}]),
    {ok, Ftp1} = ftp:open({127,0,0,1}, [{port,2021}]),
    try
	{ok, Pid2} = inets:start(ftpd, [{bind_address, {127,0,0,1}}, {port, 2121}]),
	{ok, Ftp2} = ftp:open({127,0,0,1}, [{port,2121}]),
	ok = ftp:close(Ftp2),
	inets:stop(ftpd, Pid2)
    after
	ok = ftp:close(Ftp1),
	inets:stop(ftpd, Pid1)
    end.

connect_v6_test(doc) ->
    ["Test that we can connect to the ftp server via IPv6"];
connect_v6_test(suite) ->
    [];
connect_v6_test(_Config) ->
    {ok, Pid} = inets:start(ftpd, [{bind_address, "::1"}, {port, 2021}]),
    try
	{ok, Ftp} = ftp:open({0,0,0,0,0,0,0,1}, [{port,2021}, {ipfamily, inet6}]),
	ok = ftp:close(Ftp)
    after
	inets:stop(ftpd, Pid)
    end.

info_test(Config) ->
    Pid = ?config(ftpd_pid, Config),
    Info = inets:services_info(),
    {value, {ftpd, Pid, _}} = lists:keysearch(ftpd, 1, Info).

fd_test(doc) ->
    ["Test that we can pass a file descriptor to FTP server"];
fd_test(suite) ->
    [];
fd_test(_Config) ->
    {ok, FD} = fd_nif:get_fd(),
    try
	{ok, Pid} = inets:start(ftpd, [{fd, FD}]),
	try
	    {ok, Ftp} = ftp:open("localhost", [{port,2021}]),
	    ok = ftp:close(Ftp)
	after
	    inets:stop(ftpd, Pid)
	end
    after
	% hack to close the file descriptor in case the test breaks
	fd_nif:close_fd(FD)
    end.

pwdfun(?USER, ?PASS) -> authorized;
pwdfun(_, _) -> not_authorized.

login_success_test(doc) ->
    ["Test that a user can login to the FTP server"];
login_success_test(suite) ->
    [];
login_success_test(_Config) ->
    {ok, Ftp} = ftp:open("localhost", [{port, 2021}]),
    ok = ftp:user(Ftp, ?USER, ?PASS),
    ftp:close(Ftp).

login_failure_test(doc) ->
    ["Test that a user can not login to the FTP server with wrong password"];
login_failure_test(suite) ->
    [];
login_failure_test(_Config) ->
    {ok, Ftp} = ftp:open("localhost", [{port, 2021}]),
    {error, euser} = ftp:user(Ftp, ?USER, ?WRONGPASS),
    ftp:close(Ftp).

ls_test(doc) ->
    ["Test that the user can list the current directory"];
ls_test(suite) ->
    [];
ls_test(Config) ->
    Ftp = ?config(ftp_pid, Config),
    {ok, LsRoot} = ftp:ls(Ftp),
    Lst=re:split(LsRoot, "\\r\\n", [trim]),
    [Dir, Empty, EmptyDir] = Lst,
    match = re:run(Dir, "^d.*\sdir$", [{capture, none}]),
    match = re:run(Empty, "^-.*\s\0\s+\S+\s+\S+\s+\S+\s+\d+:\d+\s+empty$", [{capture, none}]),
    match = re:run(EmptyDir, "^d.*\sempty_dir$", [{capture, none}]).

ls_dir_test(doc) ->
    ["Test that the user can list a directory"];
ls_dir_test(suite) ->
    [];
ls_dir_test(Config) ->
    Ftp = ?config(ftp_pid, Config),
    {ok, LsDir} = ftp:ls(Ftp, "dir"),
    [OneTwoThree]=re:split(LsDir, "\\r\\n", [trim]),
    match = re:run(OneTwoThree, "^-.*123$", [{capture, none}]).

ls_empty_dir_test(doc) ->
    ["Test that the user can list an empty directory"];
ls_empty_dir_test(suite) ->
    [];
ls_empty_dir_test(Config) ->
    Ftp = ?config(ftp_pid, Config),
    {ok, LsEmpty} = ftp:ls(Ftp, "empty_dir"),
    [<<>>]=re:split(LsEmpty, "\\r\\n", [trim]).

nlist_test(doc) ->
    ["Test that the user can list the files from the current directory"];
nlist_test(suite) ->
    [];
nlist_test(Config) ->
    Ftp = ?config(ftp_pid, Config),
    {ok, LsRoot} = ftp:nlist(Ftp),
    Lst=re:split(LsRoot, "\\r\\n", [trim]),
    [<<"dir">>, <<"empty">>, <<"empty_dir">>] = Lst.

nlist_file_test(doc) ->
    ["Test that the user can list a file from the current directory"];
nlist_file_test(suite) ->
    [];
nlist_file_test(Config) ->
    Ftp = ?config(ftp_pid, Config),
    {ok, LsRoot} = ftp:nlist(Ftp, "empty"),
    Lst=re:split(LsRoot, "\\r\\n", [trim]),
    [<<"empty">>] = Lst,
    {ok, LsDir} = ftp:nlist(Ftp, "dir/123"),
    LstDir=re:split(LsDir, "\\r\\n", [trim]),
    [<<"dir/123">>] = LstDir,
    {ok, LsAbsDir} = ftp:nlist(Ftp, "/dir/123"),
    LstAbsDir=re:split(LsAbsDir, "\\r\\n", [trim]),
    [<<"/dir/123">>] = LstAbsDir.

cd_test(doc) ->
    ["Test that the user can change a directory"];
cd_test(suite) ->
    [];
cd_test(Config) ->
    Ftp = ?config(ftp_pid, Config),
    ok = ftp:cd(Ftp, "dir"),
    {ok, LsDir} = ftp:ls(Ftp),
    [_]=re:split(LsDir, "\\r\\n", [trim]),
    ok = ftp:cd(Ftp, ".."),
    ls_test(Config).

pwd_test(doc) ->
    ["Test that the user can list the current working directory"];
pwd_test(suite) ->
    [];
pwd_test(Config) ->
    Ftp = ?config(ftp_pid, Config),
    {ok, "/"} = ftp:pwd(Ftp),
    ok = ftp:cd(Ftp, "dir"),
    {ok, "/dir"} = ftp:pwd(Ftp).

cd_ls_test(doc) ->
    ["Test that the ls command works from different directories"];
cd_ls_test(suite) ->
    [];
cd_ls_test(Config) ->
    Ftp = ?config(ftp_pid, Config),
    {ok, LsRoot} = ftp:ls(Ftp),
    ok = ftp:cd(Ftp, "dir"),
    {ok, LsRoot} = ftp:ls(Ftp, ".."),
    {ok, LsRoot} = ftp:ls(Ftp, "/").

cd_nlist_test(doc) ->
    ["Test that the nlst command works from different directories"];
cd_nlist_test(suite) ->
    [];
cd_nlist_test(Config) ->
    Ftp = ?config(ftp_pid, Config),
    {ok, LsRoot} = ftp:nlist(Ftp),
    [<<"dir">>, <<"empty">>, <<"empty_dir">>] = re:split(LsRoot, "\\r\\n", [trim]),
    {ok, LsAbsRoot} = ftp:nlist(Ftp, "/"),
    [<<"/dir">>, <<"/empty">>, <<"/empty_dir">>] = re:split(LsAbsRoot, "\\r\\n", [trim]),
    ok = ftp:cd(Ftp, "dir"),
    {ok, LsRelRoot} = ftp:nlist(Ftp, ".."),
    [<<"../dir">>, <<"../empty">>, <<"../empty_dir">>] = re:split(LsRelRoot, "\\r\\n", [trim]),
    {ok, LsAbsRoot} = ftp:nlist(Ftp, "/").

download_test(doc) ->
    ["Test that the user can download files."];
download_test(suite) ->
    [];
download_test(Config) ->
    Ftp = ?config(ftp_pid, Config),
    PrivDir = ?config(priv_dir, Config),
    ftp:lcd(Ftp, PrivDir),
    ok = ftp:recv(Ftp, "empty"),
    ok = ftp:recv(Ftp, "dir/123", "123"),
    {ok, <<"abc\n">>} = file:read_file(filename:join(PrivDir, "123")),
    ok = ftp:recv(Ftp, "/dir/123", "123"),
    {ok, <<>>} = file:read_file(filename:join(PrivDir, "empty")),
    {ok, <<"abc\n">>} = file:read_file(filename:join(PrivDir, "123")).

upload_test(doc) ->
    ["Test that the user can upload files."];
upload_test(suite) ->
    [];
upload_test(Config) ->
    Ftp = ?config(ftp_pid, Config),
    PrivDir = ?config(priv_dir, Config),
    DataDir = ?config(data_dir, Config),
    EmptyFileName = ?config(empty_file_name, Config),
    DataFileName = ?config(data_file_name, Config),
    ftp:lcd(Ftp, PrivDir),
    ok = ftp:send(Ftp, EmptyFileName),
    ok = ftp:send(Ftp, filename:join(PrivDir, DataFileName), DataFileName),
    {ok, <<>>} = file:read_file(filename:join(DataDir, EmptyFileName)),
    {ok, <<"ABC">>} = file:read_file(filename:join(DataDir, DataFileName)).

mkdir_test(doc) ->
    ["Test that the user can create a directory and upload a file there."];
mkdir_test(suite) ->
    [];
mkdir_test(Config) ->
    Ftp = ?config(ftp_pid, Config),
    PrivDir = ?config(priv_dir, Config),
    DataDir = ?config(data_dir, Config),
    DataFileName = ?config(data_file_name, Config),
    RemoteDir = ?config(remote_dir, Config),
    ftp:lcd(Ftp, PrivDir),
    ftp:mkdir(Ftp, RemoteDir),
    ok = ftp:send(Ftp, filename:join(PrivDir, DataFileName), filename:join(RemoteDir, DataFileName)),
    {ok, <<"ABC">>} = file:read_file(filename:join([DataDir, RemoteDir, DataFileName])).

log_trace_test(doc) ->
    ["Check that logging and tracing works"];
log_trace_test(suite) ->
    [];
log_trace_test(Config) ->
    Tid = ?config(table, Config),
    [{_, "User "++?USER++" successfully logged in."}] = ets:lookup(Tid, ?LOGIN_OK),
    [] = ets:lookup(Tid, ?LOGIN_FAIL),
    Ftp = ?config(ftp_pid, Config),
    PrivDir = ?config(priv_dir, Config),
    ftp:lcd(Ftp, PrivDir),
    ok = ftp:cd(Ftp, "dir"),
    [{_, "Changed directory to /dir"}] = ets:lookup(Tid, ?CWD),
    _ = ftp:ls(Ftp),
    [{_, "Listed directory /dir"}] = ets:lookup(Tid, ?LIST),
    ok = ftp:recv(Ftp, "123"),
    [{_, "File /dir/123 downloaded to" ++ _}] = ets:lookup(Tid, ?RETR),
    ftp_close(Config),
    timer:sleep(10),
    [{_, "User "++?USER++" logged out."}] = ets:lookup(Tid, ?CONN_CLOSE).

split_command_test(doc) ->
    ["Check that the FTP server handles commands split by TCP"];
split_command_test(suite) ->
    [];
split_command_test(Config) ->
    Sock = ?config(sock, Config),
    {ok, "220 " ++ _} = gen_tcp:recv(Sock, 0),
    ok = gen_tcp:send(Sock, "US"),
    timer:sleep(1000),
    ok = gen_tcp:send(Sock, "ER ftp\r\n"),
    {ok, "331 " ++ _} = gen_tcp:recv(Sock, 0).

cd_up_from_root_test(doc) ->
    ["Check that the FTP server doesn't handle changing directory up from root"];
cd_up_from_root_test(suite) ->
    [];
cd_up_from_root_test(Config) ->
    Ftp = ?config(ftp_pid, Config),
    {ok, "/"} = ftp:pwd(Ftp),
    ok = ftp:cd(Ftp, ".."),
    {ok, "/"} = ftp:pwd(Ftp),
    ok = ftp:cd(Ftp, "../.."),
    {ok, "/"} = ftp:pwd(Ftp),
    ok = ftp:cd(Ftp, "dir/../.."),
    {ok, "/"} = ftp:pwd(Ftp),
    ok = ftp:cd(Ftp, "./.."),
    {ok, "/"} = ftp:pwd(Ftp).

cd_over_symlink_test(doc) ->
    ["Check that the FTP server doesn't handle following symlinks out from the chroot"];
cd_over_symlink_test(suite) ->
    [];
cd_over_symlink_test(Config) ->
    Ftp = ?config(ftp_pid, Config),
    {ok, "/"} = ftp:pwd(Ftp),
    {error, _} = ftp:cd(Ftp, "tmp"),
    {ok, "/"} = ftp:pwd(Ftp),
    {error, _} = ftp:cd(Ftp, "/tmp"),
    {ok, "/"} = ftp:pwd(Ftp),
    {error, _} = ftp:cd(Ftp, "./tmp"),
    {ok, "/"} = ftp:pwd(Ftp),
    {error, _} = ftp:cd(Ftp, "dir/../tmp"),
    {ok, "/"} = ftp:pwd(Ftp).

unreadable_dir_test(doc) ->
    ["Check that the FTP server doesn't list unreadable directory"];
unreadable_dir_test(suite) ->
    [];
unreadable_dir_test(Config) ->
    Ftp = ?config(ftp_pid, Config),
    UnreadDir = ?config(un_name, Config),
    % Intentional: both the Solaris and Linux FTP servers seem to
    % return empty data on error instead of error codes
    {ok, []} = ftp:nlist(Ftp, UnreadDir).

unreadable_file_test(doc) ->
    ["Check that the FTP server doesn't let download an unreadable file"];
unreadable_file_test(suite) ->
    [];
unreadable_file_test(Config) ->
    Ftp = ?config(ftp_pid, Config),
    UnreadFile = ?config(un_name, Config),
    {error, _} = ftp:recv(Ftp, UnreadFile).

unwritable_dir_test(doc) ->
    ["Check that the FTP server doesn't let write to an unwritable directory"];
unwritable_dir_test(suite) ->
    [];
unwritable_dir_test(Config) ->
    Ftp = ?config(ftp_pid, Config),
    UnwriteDir = ?config(un_name, Config),
    File = ?config(file_to_rename, Config),
    {error, _} = ftp:send_bin(Ftp, <<"abc">>, filename:join([UnwriteDir, "tmp"])),
    ok = ftp:cd(Ftp, "tmp"),
    {error, _} = ftp:rename(Ftp, File, "test").

unwritable_file_test(doc) ->
    ["Check that the FTP server doesn't let write to an unwritable file"];
unwritable_file_test(suite) ->
    [];
unwritable_file_test(Config) ->
    Ftp = ?config(ftp_pid, Config),
    UnwriteFile = ?config(un_name, Config),
    {error, _} = ftp:send_bin(Ftp, <<"abc">>, UnwriteFile).

read_dir_test(doc) ->
    ["Check that the FTP server doesn't let download a directory instead of a file"];
read_dir_test(suite) ->
    [];
read_dir_test(Config) ->
    Ftp = ?config(ftp_pid, Config),
    {error, _} = ftp:recv(Ftp, "dir").

write_dir_test(doc) ->
    ["Check that the FTP server doesn't let upload a directory instead of a file"];
write_dir_test(suite) ->
    [];
write_dir_test(Config) ->
    Ftp = ?config(ftp_pid, Config),
    {error, _} = ftp:send_bin(Ftp, <<"abc">>, "dir").


logfun(?LOGIN_OK=Event, [UserName]) ->
    ets:insert(logtrace, {Event, "User "++UserName++" successfully logged in."});
logfun(?LOGIN_FAIL=Event, [UserName]) ->
    ets:insert(logtrace, {Event, "User "++UserName++" failed to log in."});
logfun(?CONN_CLOSE=Event, [UserName]) ->
    ets:insert(logtrace, {Event, "User "++UserName++" logged out."});
logfun(Event, Params) ->
    ets:insert(logtrace, f("Unknown log, event: ~p, params: ~p", [Event, Params])).

tracefun(?CWD=Event, [Dir]) ->
    ets:insert(logtrace, {Event, "Changed directory to "++Dir});
tracefun(?RETR=Event, [FileOnServer, FileOnClient]) ->
    ets:insert(logtrace, {Event, "File "++FileOnServer++" downloaded to "++FileOnClient});
tracefun(?STOR=Event, [FileOnServer, FileOnClient]) ->
    ets:insert(logtrace, {Event, "File "++FileOnClient++" uploaded to "++FileOnServer});
tracefun(?LIST=Event, [Dir]) ->
    ets:insert(logtrace, {Event, "Listed directory "++Dir});
tracefun(Event, Params) ->
    ets:insert(logtrace, f("Unknown trace, event: ~p, params: ~p", [Event, Params])).

f(Format, Params) ->
    lists:flatten(io_lib:format(Format, Params)).

ftp_connect(Config) ->
    FtpHost = ?config(ftp_server_address, Config),
    IpFamily = case inet:getaddr(FtpHost, inet) of
	{ok, _} ->
	    [];
	_ -> % this is just test code, I suppose only exiting addresses are used
	    [{ipfamily, inet6}]
    end,
    {ok, Ftp} = ftp:open(FtpHost, [{port, 2021} | IpFamily]),
    ok = ftp:user(Ftp, ?USER, ?PASS),
    [{ftp_pid, Ftp} | Config].

ftp_close(Config) ->
    Ftp = ?config(ftp_pid, Config),
    ftp:close(Ftp).

chunk_test(doc) ->
    ["Test that the user can download files in chunks."];
chunk_test(suite) ->
    [];
chunk_test(Config) ->
    Ftp = ?config(ftp_pid, Config),
    PrivDir = ?config(priv_dir, Config),
    ftp:lcd(Ftp, PrivDir),
	ok = ftp:recv_chunk_start(Ftp, "dir/123"),
	{ok, <<"abc\n">>} = ftp:recv_chunk(Ftp),
	ok = ftp:recv_chunk(Ftp).
