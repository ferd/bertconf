%% Tests to check whether we can parse a given BERT file correctly
%% with the right expected format. Includes tests for file changes
%% detection
%% Tests bertconf_bert_loader.erl
-module(bertconf_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).
-export([access_table/1]).

all() -> [access_table].

init_per_suite(Config) ->
    application:load({application, bertconf, []}),
    Config.

end_per_suite(_Config) -> ok.

init_per_testcase(_Name, Config) ->
   % Data = ?config(data_dir, Config),
   % Priv = ?config(priv_dir, Config),
   % {ok, Files} = file:list_dir(Data),
   % [{ok,_}=file:copy(filename:join(Data,Name), filename:join(Priv,Name)) ||
   %     Name <- Files],
    application:set_env(bertconf, dir, [?config(data_dir, Config)]),
    application:set_env(bertconf, delay, 500),
    %% for some reason, application:start works like shit in here.
    % application:start(bertconf),
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
