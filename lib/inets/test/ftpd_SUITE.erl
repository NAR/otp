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

%% Test server specific exports
-export([all/0, suite/0,groups/0,init_per_group/2,end_per_group/2]).
-export([init_per_testcase/2, end_per_testcase/2,
	 init_per_suite/1, end_per_suite/1]).

-export([start_stop_test/1,
	 connect_test/1,
	 multiple_servers_test/1,
	 connect_v6_test/1,
     	 login_success_test/1,
     	 login_failure_test/1,
	 ls_test/1,
	 ls_dir_test/1,
	 ls_empty_dir_test/1,
	 cd_test/1,
	 download_test/1,
	 upload_test/1,
	 fd_test/1
	]).

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
suite() -> [].

all() -> [
	{group, basic_tests}, 
	{group, login_tests}, 
	{group, directory_tests},
    	{group, download_upload_tests},
        {group, ipv6_tests}
    ].

groups() ->
    [{basic_tests, [], [start_stop_test, connect_test, multiple_servers_test, connect_v6_test, fd_test]},
     {login_tests, [], [login_success_test, login_failure_test]},
     {directory_tests, [parallel], [ls_test, ls_dir_test, ls_empty_dir_test, cd_test]},
     {download_upload_tests, [], [download_test, upload_test]},
     {ipv6_tests, [], [ls_test, ls_dir_test, ls_empty_dir_test, cd_test, download_test, upload_test]}
    ].

init_per_suite(Config) ->
    ok = inets:start(),
    Config.

end_per_suite(Config) ->
    inets:stop(),
    Config.

init_per_group(basic_tests, Config) ->
    Config;
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
	io:write(almalma),
	io:write(DataDir),
    {ok, Pid} = inets:start(ftpd, [{port, 2021}, {pwd_fun, fun pwdfun/2}, {chrootDir, DataDir}]),
    [{ftpd_pid, Pid}, {ftp_server_address, "localhost"} | Config].

end_per_group(basic_tests, Config) ->
    Config;
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

end_per_testcase(upload_test, Config) ->
    % Remove the uploaded files
    DataDir = ?config(data_dir, Config),
    EmptyFileName = ?config(empty_file_name, Config),
    DataFileName = ?config(data_file_name, Config),
    file:delete(filename:join(DataDir, EmptyFileName)),
    file:delete(filename:join(DataDir, DataFileName)),
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
    {ok, Pid} = inets:start(ftpd, [{bind_address, {0,0,0,0,0,0,0,1}}, {port, 2021}]),
    try
	{ok, Ftp} = ftp:open({0,0,0,0,0,0,0,1}, [{port,2021}, {ipfamily, inet6}]),
	ok = ftp:close(Ftp)
    after
	inets:stop(ftpd, Pid)
    end.

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

pwdfun("test", "test") -> authorized;
pwdfun(_, _) -> not_authorized.

login_success_test(doc) ->
    ["Test that a user can login to the FTP server"];
login_success_test(suite) ->
    [];
login_success_test(_Config) ->
    {ok, Ftp} = ftp:open("localhost", [{port, 2021}]),
    ok = ftp:user(Ftp, "test", "test"),
    ftp:close(Ftp).

login_failure_test(doc) ->
    ["Test that a user can not login to the FTP server with wrong password"];
login_failure_test(suite) ->
    [];
login_failure_test(_Config) ->
    {ok, Ftp} = ftp:open("localhost", [{port, 2021}]),
    {error, euser} = ftp:user(Ftp, "test", "test2"),
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
	io:write(Empty),
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
    []=re:split(LsEmpty, "\\r\\n", [trim]).

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

download_test(doc) ->
    ["Test that the user can download files."];
download_test(suite) ->
    [];
download_test(Config) ->
    Ftp = ?config(ftp_pid, Config),
    PrivDir = ?config(priv_dir, Config),
    ftp:lcd(Ftp, PrivDir),
    ok = ftp:recv(Ftp, "empty"),
    ok = ftp:recv(Ftp, "dir/123"),
    {ok, <<>>} = file:read_file(filename:join(PrivDir, "empty")),
    {ok, <<"abc">>} = file:read_file(filename:join(PrivDir, "123")).

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
    ok = ftp:send(Ftp, filename:join(PrivDir, EmptyFileName)),
    ok = ftp:send(Ftp, filename:join(PrivDir, DataFileName)),
    {ok, <<>>} = file:read_file(filename:join(DataDir, EmptyFileName)),
    {ok, <<"ABC">>} = file:read_file(filename:join(DataDir, DataFileName)).

ftp_connect(Config) ->
    FtpHost = ?config(ftp_server_address, Config),
    IpFamily = case inet:getaddr(FtpHost, inet) of
	{ok, _} -> 
	    [];
	_ -> % this is just test code, I suppose only exiting addresses are used
	    [{ipfamily, inet6}]
    end,
    {ok, Ftp} = ftp:open(FtpHost, [{port, 2021} | IpFamily]),
    ok = ftp:user(Ftp, "test", "test"),
    [{ftp_pid, Ftp} | Config].

ftp_close(Config) ->
    Ftp = ?config(ftp_pid, Config),
    ftp:close(Ftp).

