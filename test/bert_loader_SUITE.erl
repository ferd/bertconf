%% Tests to check whether we can parse a given BERT file correctly
%% with the right expected format. Includes tests for file changes
%% detection
%% Tests bertconf_bert_loader.erl
-module(bert_loader_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([read_files_and_tables/1, not_bert_ignored/1, observe_changes/1]).

-define(TABLE, bertconf_bert_loader).
-record(tab, {name, id}).


all() -> [read_files_and_tables, not_bert_ignored, observe_changes].

init_per_testcase(observe_changes, Config) ->
    Data = ?config(data_dir, Config),
    Priv = ?config(priv_dir, Config),
    {ok, Files} = file:list_dir(Data),
    [{ok,_}=file:copy(filename:join(Data,Name), filename:join(Priv,Name)) ||
        Name <- Files],
    application:set_env(bertconf, dir, [Priv]),
    application:set_env(bertconf, delay, 500),
    bertconf_bert_loader:start_link(),
    Config;
init_per_testcase(_Name, Config) ->
    Data = ?config(data_dir, Config),
    application:set_env(bertconf, dir, [Data]),
    bertconf_bert_loader:start_link(),
    Config.

end_per_testcase(_Name, _Config) ->
    ok.

%% The module creates tables and accessor functions accordingly to files
%% found in a directory.
%% Note: the expected behaviour is that config files are read once as part of the
%% startup procedure.
read_files_and_tables(Config) ->
    %% tables would be placement & format, both public and retrievable
    %% from an index table by the name of the module. An accessor module
    %% should later be created to abstract this away
    [_|_] = ets:tab2list(?TABLE),
    [#tab{id=Placement}] = ets:lookup(?TABLE, placement),
    [#tab{id=Format}] = ets:lookup(?TABLE, format),
    [_|_] = ets:tab2list(Placement),
    [_|_] = ets:tab2list(Format).


%% Only ".bert" files are inspected -- the rest, even if subscribing to
%% the BERT format is not visible in tables
not_bert_ignored(Config) ->
    %% A value called 'not_bert' has been inserted in a .not_bert
    %% file under the namespace 'format'. If it's found, we failed.
    %% This test should pass unless bertconf_lib's suite is also failing.
    [#tab{id=Tid}] = ets:lookup(?TABLE, format),
    [] = ets:lookup(Tid, not_bert).

%% Changes to a file every N minutes or so make the db updated
 % - central config lookup table. When swapping two tables, keep one of them
 %   running in the background, a bit like Erlang's module purge system.
 %   make it read_optimized and commit all writes at once.
observe_changes(Config) -> 
    [#tab{id=Placement}] = ets:lookup(?TABLE, placement),
    [#tab{id=Format1}] = ets:lookup(?TABLE, format),
    [] = ets:lookup(Format1, custom_test_entry),
    [_|_] = ets:tab2list(Format1),
    %% change files
    Dir = ?config(priv_dir, Config),
    File = filename:join(Dir, "rtb_state.bert"),
    {ok, Bin} = file:read_file(File),
    [{placement,_}, {format, L}] = binary_to_term(Bin),
    NewList = [{format, [{custom_test_entry, 1}|L]}, {newtable, [{a,b}]}],
    file:write_file(File, term_to_binary(NewList)),
    %% wait for reload
    timer:sleep(750),
    [#tab{id=Format2}] = ets:lookup(?TABLE, format),
    true = Format1 =/= Format2,
    [#tab{}] = ets:lookup(?TABLE, newtable),
    [{custom_test_entry,1}] = ets:lookup(Format2, custom_test_entry),
    %% Old tables are deleted if they were replaced, but not otherwise and not
    %% on the first reload
    [#tab{id=Placement}] = ets:lookup(?TABLE, placement),
    [_|_] = ets:info(Format1),
    %% wait for reload
    timer:sleep(750),
    undefined = ets:info(Format1).
