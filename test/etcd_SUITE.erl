- module(etcd_SUITE).

- compile(export_all).

all() ->
    [
        set_value,
        get_value,
        watch_value,
        delete_value
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(etcd),
    Config.


set_value(_) ->
    {ok,_ } = etcd:set("/message", "1").

get_value(_) ->
    {ok, <<"1">>} = etcd:get("/message").

watch_value(_) ->
    CallBack = fun(V) ->
        {ok, _ } = etcd:set("/test", "1"),
        ct:log("the output is :~p", V)
    end,
    ok = etcd:watch("/message", CallBack),

    {ok,_ } = etcd:set("/message", "2"),
    timer:sleep(2000),

    {ok, <<"1">>} = etcd:get("/test"),
    {ok, _} = etcd:delete("/test").

delete_value(_) ->
    {ok, _} = etcd:delete("/message").
