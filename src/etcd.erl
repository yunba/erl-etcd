-module(etcd).

-export([set/3, set/2, get/1, delete/1, watch/2]).

-spec set(Key::list(), Value::list(), TTL::integer()) -> ok.
set(Key, Value, TTL) ->
    gen_server:call(etcd_worker, {set, Key, Value, TTL}).

-spec set(Key::list(), Value::list()) -> ok.
set(Key, Value ) ->
    gen_server:call(etcd_worker, {set, Key, Value}).

-spec get(Key::list()) -> {ok, list()}.
get(Key) ->
    gen_server:call(etcd_worker, {get, Key}).

-spec delete(Key::list()) -> ok.
delete(Key) ->
    gen_server:call(etcd_worker, {delete, Key}).

-spec watch(Key::list(), Callback::fun((binary())->ok)) -> ok.
watch(Key, Callback) ->
    gen_server:cast(etcd_worker, {watch, Key, Callback}).
