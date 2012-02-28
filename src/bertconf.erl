-module(bertconf).
-behaviour(application).
-export([start/2, stop/1]).
-export([read/2]).
-define(TABLE, bertconf_bert_loader).
-include("bertconf.hrl").

%%% APPLICATION CALLBACKS %%%
start(normal, _) ->
    bertconf_sup:start_link().

stop(_) -> ok.


%%% PUBLIC INTERFACE %%%
read(NameSpace, Key) ->
    [#tab{id=Tid}] = ets:lookup(?TABLE, NameSpace),
    case ets:lookup(Tid, Key) of
        [{_Key,Val}] -> {ok, Val};
        [] -> undefined
    end.

