- module(etcd_SUITE).

- compile(export_all).

all() ->
    [
        set_value,
        set_value_with_ttl,
        refresh_value_with_ttl,
        get_value,
        watch_value,
        delete_value
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(etcd),
    Config.


set_value(_) ->
    {ok,_ } = etcd:set("/message", "1").

set_value_with_ttl(_) ->
    {ok,_ } = etcd:set("/ttlmsg", "1", 1),
    timer:sleep(2000),
    {fail, not_found} = etcd:get("/ttlmsg"),
    ok.

refresh_value_with_ttl(_) ->
    {ok,_ } = etcd:set("/ttlmsg", "1", 1),
    timer:sleep(500),
    {ok,_ } = etcd:set("/ttlmsg", "", 2),
    timer:sleep(500),
    {ok, _} = etcd:get("/ttlmsg").

get_value(_) ->
    {ok, <<"1">>} = etcd:get("/message").

watch_value(_) ->
    CallBack = fun(V) ->
        {ok, _ } = etcd:set("/test", "1"),
        ct:log("the output is :~p", V),
        ok
    end,
    ok = etcd:watch("/message", CallBack),

    {ok,_ } = etcd:set("/message", "2"),
    timer:sleep(2000),

    {ok, <<"1">>} = etcd:get("/test"),
    {ok, _} = etcd:delete("/test").

delete_value(_) ->
    {ok, _} = etcd:delete("/message").
