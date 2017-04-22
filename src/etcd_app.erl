-module(etcd_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    prepare_cache_ets(),
    etcd_sup:start_link().

prepare_cache_ets() ->
    ets:new(etcd_ets_cache, [named_table, public, set]).

stop(_State) ->
    ok.
