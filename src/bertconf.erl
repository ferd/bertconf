-module(bertconf).
-behaviour(application).
-export([start/2, stop/1]).
-export([read/2, all/1]).
-define(TABLE, bertconf_bert_loader).
-define(MATCH_LIMIT, 500).
-include("bertconf.hrl").

%%% APPLICATION CALLBACKS %%%
start(normal, _) ->
    bertconf_sup:start_link().

stop(_) -> ok.


%%% PUBLIC INTERFACE %%%
read(NameSpace, Key) ->
    case ets:lookup(table(NameSpace), Key) of
        [{_Key,Val}] -> {ok, Val};
        [] -> undefined
    end.

all(NameSpace) ->
    loop_all({table(NameSpace), '_', ?MATCH_LIMIT}).

%%% PRIVATE
table(NameSpace) ->
    [#tab{id=Tid}] = ets:lookup(?TABLE, NameSpace),
    Tid.

loop_all('$end_of_table') ->
    [];
loop_all({Match, Continuation}) ->
    [Match | loop_all(ets:match_object(Continuation))];
loop_all({Tid, Pat, Limit}) ->
    lists:append(loop_all(ets:match_object(Tid, Pat, Limit))).
