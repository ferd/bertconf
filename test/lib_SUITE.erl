%% Tests to check whether we can parse a given BERT file correctly
%% with the right expected format. Includes tests for file changes
%% detection
%% Tests bertconf_lib.erl
-module(lib_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

-export([check_format/1, find_bert/1, inspect_changes/1]).

all() -> [check_format, find_bert, inspect_changes].

init_per_suite(Config) ->
    Dir = ?config(data_dir, Config),
    [{old, filename:join(Dir, "old_rtb_state.bert")},
     {current, filename:join(Dir, "rtb_state.bert")},
     {not_bert, filename:join(Dir, "rtb_state.not_bert")} | Config].

end_per_suite(_Config) -> ok.

init_per_testcase(check_format, Config) ->
    Old = ?config(old, Config),
    Current = ?config(current, Config),
    [{old, element(2,file:read_file(Old))},
     {current, element(2,file:read_file(Current))} | Config];
init_per_testcase(inspect_changes, Config) ->
    Data = ?config(data_dir, Config),
    Priv = ?config(priv_dir, Config),
    {ok, Files} = file:list_dir(Data),
    [{ok,_}=file:copy(filename:join(Data,Name), filename:join(Priv,Name)) ||
        Name <- Files],
    Config;
init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) -> ok.

%% The accepted format is of the form [{namespace, [{Key, Val}]}]
check_format(Config) ->
    NameSpaced = fun({Atom, List}) -> is_atom(Atom) andalso is_list(List);
                    (_) -> false
                 end,
    KeyVal = fun({_Key, _Val}) -> true;
                (_) -> false
             end,
    case bertconf_lib:decode(?config(current, Config)) of
        {ok, Terms} ->
            true = lists:all(NameSpaced, Terms),
            true = lists:all(KeyVal, Terms);
        {error, _R} ->
            error({invalid_error, _R, binary_to_term(?config(current,Config))})
    end,
    case bertconf_lib:decode(?config(old, Config)) of
        {ok, _} -> erlang:error(decoded_invalid);
        {error, bad_config_format} -> ok
    end.

%% Returns all ".bert" file in a directory
find_bert(Config) ->
    Dir = ?config(data_dir, Config),
    true =
        lists:sort([?config(old,Config),?config(current,Config)])
        =:=
        lists:sort(bertconf_lib:find_bert_files(Dir)).

%% Tell what files changed or not.
inspect_changes(Config) ->
    Dir = ?config(priv_dir, Config),
    {Changed1, Refs1} = bertconf_lib:inspect(Dir),
    2 = length(Changed1),
    2 = length(Refs1),
    modify_dir(Dir),
    {Changed2, Refs2} = bertconf_lib:inspect(Dir,Refs1),
    1 = length(Changed2),
    2 = length(Refs2),
    {[], Refs2} = bertconf_lib:inspect(Dir,Refs2). % shouldn't change.

%%% UTILITY FUNCTIONS %%%
modify_dir(Dir) ->
    [H|_] = bertconf_lib:find_bert_files(Dir),
    file:write_file(H, term_to_binary(gotcha)),
    ok.
