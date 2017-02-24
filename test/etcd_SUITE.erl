- module(etcd_SUITE).

- compile(export_all).

all() ->
    [
        set_value,
        set_value_with_ttl,
        refresh_value_with_ttl,
        get_value,
        watch_value,
        watch_dir,
        get_dir,
        delete_value
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(etcd),
    etcd:delete("/message"),
    etcd:delete("/ttlmsg"),
    etcd:delete("/test1"),
    etcd:delete("/test2"),
    etcd:delete("/test_dir"),
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
    {ok, _} = get_one_node_value("/ttlmsg").

get_value(_) ->
    {ok, <<"1">>} = get_one_node_value("/message").

watch_value(_) ->
    %% this test is a little care about timing...
    Callback = fun(_V) ->
        {ok, _ } = etcd:set("/test1", "1"),
        ok
    end,
    ok = etcd:watch("/message", Callback),
    timer:sleep(1000),

    {ok,Msg } = etcd:set("/message", "2"),
    ct:log(Msg),
    timer:sleep(1000),

    %% stop after it's triggered
    {ok, <<"1">>} = get_one_node_value("/test1"),

    SecondCallback = fun(_V) ->
        {ok, _} = etcd:set("/test2", "1"),
        stop
    end,

    ok = etcd:watch("/test1", SecondCallback),
    timer:sleep(1000),

    {ok,_ } = etcd:set("/message", "3"),

    timer:sleep(1000),
    %% /message changed -> trigger /test chagning -> trigger /test2 changing
    {ok, <<"1">>} = get_one_node_value("/test2"),
    {ok, _} = etcd:delete("/test1"),
    {ok, _} = etcd:delete("/test2").

watch_dir(_) ->
    CallBack = fun(V) ->
        {ok, _ } = etcd:set("/dir_test", "1"),
        ct:log("the output is :~p", V),
        ok
    end,
    ok = etcd:watch_dir("/test_dir", CallBack),

    {ok,_ } = etcd:set("/test_dir/hello", "2"),
    {ok,_ } = etcd:set("/test_dir/hello2", "2"),
    timer:sleep(2000),

    %% stop after it's triggered
    {ok, <<"1">>} = get_one_node_value("/dir_test").

get_dir(_) ->
    {ok, Body} = etcd:get("/test_dir"),
    ct:log(Body),
    ok.

delete_value(_) ->
    {ok, _} = etcd:delete("/message").

get_one_node_value(Key) ->
    {ok, Body} = etcd:get(Key),
    get_value_from_response_body(Body).
    
get_value_from_response_body(Body) ->
    case jiffy:decode(Body) of
        {Props} ->
            %% no error, return value
            {NodeValue} = proplists:get_value(<<"node">>, Props),
            Value = proplists:get_value(<<"value">>, NodeValue),
            {ok, Value};
        _ ->
            {fail, wrong_json_body}
    end.
