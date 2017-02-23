-module(etcd).

-export([set/3, set/2, get/1, delete/1, watch/2, watch_dir/2]).

-spec set(Key::list(), Value::list(), TTL::integer()) -> {ok, list()}.
set(Key, Value, TTL) ->
    gen_server:call(etcd_worker, {set, Key, Value, TTL}).

-spec set(Key::list(), Value::list()) -> {ok, list()}.
set(Key, Value ) ->
    gen_server:call(etcd_worker, {set, Key, Value}).

-spec get(Key::list()) -> {ok, list()}.
get(Key) ->
    gen_server:call(etcd_worker, {get, Key}).

-spec delete(Key::list()) -> {ok, list()}.
delete(Key) ->
    gen_server:call(etcd_worker, {delete, Key}).

-spec watch(Key::list(), Callback::fun((list())->(ok|stop))) -> ok.
watch(Key, Callback) ->
    gen_server:cast(etcd_worker, {watch, Key, undefined, Callback}).

-spec watch_dir(Key::list(), Callback::fun((list())->(ok|stop))) -> ok.
watch_dir(Key, Callback) ->
    gen_server:cast(etcd_worker, {watch_dir, Key, undefined, Callback}).
