-module(bertconf_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link(?MODULE, []).

init([]) ->
    ChildSpec = {bertconf_serv,
                 {bertconf_bert_loader, start_link, []},
                 permanent,
                 5000,
                 worker,
                 [bertconf_bert_loader]},
    {ok, {{one_for_one, 10, 5000}, [ChildSpec]}}.
