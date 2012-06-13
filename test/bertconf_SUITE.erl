%% Tests to check whether we can parse a given BERT file correctly
%% with the right expected format. Includes tests for file changes
%% detection
%% Tests bertconf_bert_loader.erl
-module(bertconf_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).
-export([access_table/1, all_data/1, table_version/1]).

all() -> [access_table, all_data, table_version].

init_per_suite(Config) ->
    application:load({application, bertconf, []}),
    Config.

end_per_suite(_Config) -> ok.

init_per_testcase(table_version, Config) ->
    Data = ?config(data_dir, Config),
    Priv = ?config(priv_dir, Config),
    {ok, Files} = file:list_dir(Data),
    [{ok,_}=file:copy(filename:join(Data,Name), filename:join(Priv,Name)) ||
        Name <- Files],
    application:set_env(bertconf, dir, [Priv]),
    application:set_env(bertconf, delay, 500),
    %% for some reason, application:start works like shit in here.
    % application:start(bertconf),
    {ok, Pid} = bertconf:start(normal, temporary),
    unlink(Pid),
    [{kill, Pid} | Config];
init_per_testcase(_Name, Config) ->
    application:set_env(bertconf, dir, [?config(data_dir, Config)]),
    application:set_env(bertconf, delay, 500),
    %% for some reason, application:start works like shit in here.
    {ok, Pid} = bertconf:start(normal, temporary),
    unlink(Pid),
    [{kill, Pid} | Config].

end_per_testcase(_Name, Config) ->
    %application:stop(bertconf).
    exit(?config(kill, Config), kill).

access_table(_Config) ->
    NameSpace = format,
    Key = {728,90},
    {ok, [9881]} = bertconf:read(NameSpace,Key),
    undefined = bertconf:read(NameSpace,make_ref()).

all_data(_Config) ->
    [{9880, _},
     {9881, _},
     {14611, _},
     {14612, _},
     {14613, _}] = lists:sort(bertconf:all(placement)).

table_version(Config) ->
    %% get current table version reference
    Version = bertconf:version(format),
    current = bertconf:version(format, Version),

    %% change files
    Dir = ?config(priv_dir, Config),
    File = filename:join(Dir, "rtb_state.bert"),
    {ok, Bin} = file:read_file(File),
    [{format,L},{placement,_}] = lists:sort(binary_to_term(Bin)),
    NewList = [{format, [{custom_test_entry, now()}|L]}, {newtable, [{a,b}]}],
    file:write_file(File, term_to_binary(NewList)),

    %% wait for reload
    timer:sleep(950),

    old = bertconf:version(format, Version),
    NewVersion = bertconf:version(format),
    true = NewVersion =/= Version,
    current = bertconf:version(format, NewVersion).
