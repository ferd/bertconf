-module(bertconf_bert_loader).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-include("bertconf.hrl").
-record(state, {ref, changes=[], old_tables=[]}).

%%% PUBLIC INTERFACE %%%
start_link() ->
    gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

%%% GEN_SERVER CALLBACKS %%%
init([]) ->
    process_flag(trap_exit, true),
    ?MODULE = ets:new(?MODULE, [named_table, set, public, {keypos, #tab.name}, {read_concurrency, true}]),
    {[], Changes} = reload_bert([]),
    {ok, #state{ref=erlang:start_timer(reload_delay(), self(), reload),
                changes=Changes}}.

handle_call(_Event, _From, State) ->
    {noreply, State}.

handle_cast(_Event, State) ->
    {noreply, State}.

handle_info({timeout, Ref, reload}, S=#state{ref=Ref, changes=Chg, old_tables=Old}) ->
    {OldTables, Changes} = reload_bert(Chg),
    delete_tables(Old),
    {noreply, S#state{ref=erlang:start_timer(reload_delay(), self(), reload),
                      changes=Changes,
                      old_tables=OldTables}};
handle_info(_Event, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%%%%%%%%%%%%%%%
%%% PRIVATE %%%
%%%%%%%%%%%%%%%
reload_bert(OldChanges) ->
    Dirs = find_dirs(),
    Changes = [bertconf_lib:inspect(Dir, OldChanges) || Dir <- Dirs],
    Files = lists:append([Changed || {Changed, _Refs} <- Changes]),
    Terms = [Terms || File <- Files,
                      {ok, Bin} <- [file:read_file(File)],
                      {ok, Terms} <- [bertconf_lib:decode(Bin)]],
    NewTables = store(merge(lists:sort(lists:flatten(Terms)))),
    OldTables = update_table_index(NewTables),
    NewChanges = lists:append([Refs || {_, Refs} <- Changes]),
    {OldTables, NewChanges}.

reload_delay() ->
    case application:get_env(bertconf, delay) of
        undefined -> 5000;
        {ok, Val} -> Val
    end.

find_dirs() ->
    case application:get_env(bertconf, dir) of
        undefined -> [];
        {ok, Val} -> Val
    end.

merge([]) -> [];
merge([Table]) -> [Table];
merge([{NameSpace, ListA}, {NameSpace, ListB} | Terms]) -> % identical NS.
    merge([{NameSpace, ListA++ListB} | Terms]);
merge([Table | Terms]) -> % different NS
    [Table | merge(Terms)].

store([]) -> [];
store([[]|Tables]) -> % some file not respecting the format got skipped
    store(Tables);
store([{NameSpace, Records} | Tables]) ->
    Tid = ets:new(NameSpace, [set, public, {read_concurrency, true}]),
    ets:insert(Tid, Records),
    [#tab{name=NameSpace, id=Tid} | store(Tables)].

%% We should delete old tables, but it might be safer to keep them
%% in case of programmer mistake. We're more likely to die of such a
%% mistake than we are to go out of memory because of config files.
%% However, tables that were successfully replaced should mean that
%% the old version of the table is gone.
%% The ordering of operations is important to avoid having a window
%% where some config values are unavailable.
update_table_index(NewTables) ->
    OldTables = [Tid || #tab{name=Name} <- NewTables,
                        [#tab{id=Tid}] <- [ets:lookup(?MODULE, Name)]],
    ets:insert(?MODULE, NewTables),
    %% Don't delete, keep an old generation of tables:
    %% [Current | Old | ToDelete]
    %% to avoid blackouts during changes
    OldTables.

delete_tables(OldTables) ->
    [ets:delete(Tid) || Tid <- OldTables],
    ok.
