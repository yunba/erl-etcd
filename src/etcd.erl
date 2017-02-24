-module(etcd).

-export([set/3, set/2, get/1, delete/1, watch/2, watch_dir/2]).

%%%% set up a key with value with a TTL value(in seconds).
%%%% return is {ok, response string list from etcd}
-spec set(Key::list(), Value::list(), TTL::integer()) -> {ok, list()}| {fail, Reason::atom()}.
set(Key, Value, TTL) ->
    gen_server:call(etcd_worker, {set, Key, Value, TTL}).

%%%% set up a key with value WITHOUT a TTL value.
%%%% return is {ok, response string list from etcd}
-spec set(Key::list(), Value::list()) -> {ok, list()} | {fail, Reason::atom()}.
set(Key, Value ) ->
    gen_server:call(etcd_worker, {set, Key, Value}).

%%%% get the value of a key/dir.
%%%% return is {ok, response string list from etcd}
%%%% if the key doesn't exist, return {fail, not_found}
-spec get(Key::list()) -> {ok, list()}| {fail, Reason::atom()}.
get(Key) ->
    gen_server:call(etcd_worker, {get, Key}).

%%%% delete the value of a key/dir.
%%%% return is {ok, response string list from etcd}
%%%% if the key doesn't exist, it will return {ok, _} as well.
-spec delete(Key::list()) -> {ok, list()}| {fail, Reason::atom()}.
delete(Key) ->
    gen_server:call(etcd_worker, {delete, Key}).

%%% wait for the key changing event asynchronously
%%% when the key is changed, Callback function will be called,
%%% and the input will be the response string from etcd.
%%% the Callback should return ok to continue waiting, or stop to exit the waiting.
%%% Alarm: This API won't work for dir
-spec watch(Key::list(), Callback::fun((list())->(ok|stop))) -> ok.
watch(Key, Callback) ->
    gen_server:cast(etcd_worker, {watch, Key, undefined, Callback}).

%%% Wait for the dir changing event asynchronously.
%%% when the any key in the dir is changed, Callback function will be called,
%%% and the input will be the response string from etcd.
%%% the Callback should return ok to continue waiting, or stop to exit the waiting.
-spec watch_dir(Key::list(), Callback::fun((list())->(ok|stop))) -> ok.
watch_dir(Key, Callback) ->
    gen_server:cast(etcd_worker, {watch_dir, Key, undefined, Callback}).
