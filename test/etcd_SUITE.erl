- module(etcd_SUITE).

- compile(export_all).
- include("etcd.hrl").

all() ->
    [
        gen_read_opt_querys,
        gen_write_opt_data_and_querys,
        set_value,
        set_value_with_ttl,
        refresh_value_with_ttl,
        refresh_ttl_only,
        only_work_when_prev_exist,
        only_work_when_prev_value,
        get_value,
        watch_value,
        watch_dir,
        get_dir,
        delete_value
    ].

only_work_when_prev_exist(_) ->
    etcd:delete("/prev_exist"),
    etcd:delete("/prev_exist2"),
    etcd:set("/prev_exist", "1"),
    Opts1 = #etcd_modify_opts{
        key = "/prev_exist", prev_exist = true,
        value = "2"},
    etcd:set(Opts1),
    {ok, <<"2">>} = get_one_node_value("/prev_exist"),
    Opts2 = #etcd_modify_opts{
        key = "/prev_exist2", prev_exist = true,
        value = "2"},
    etcd:set(Opts2),
    {fail, not_found} = etcd:get("/prev_exist2"),
    etcd:delete("/prev_exist"),
    ok.

only_work_when_prev_value(_) ->
    etcd:delete("/prev_value"),
    etcd:set("/prev_value", "1"),
    {ok, <<"1">>} = get_one_node_value("/prev_value"),
    Opts1 = #etcd_modify_opts{
        key = "/prev_value", prev_value = "1",
        value = "2"},
    etcd:set(Opts1),
    {ok, <<"2">>} = get_one_node_value("/prev_value"),

    Opts2 = #etcd_modify_opts{
        key = "/prev_value", prev_value = "1",
        value = "3"},
    etcd:set(Opts2),
    {ok, <<"2">>} = get_one_node_value("/prev_value"),
    ok.

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(etcd),
    etcd:delete("/message"),
    etcd:delete("/ttlmsg"),
    etcd:delete("/test1"),
    etcd:delete("/test2"),
    etcd:delete("/test_dir"),
    Config.

gen_read_opt_querys(_) ->
    ok.

gen_write_opt_data_and_querys(_) ->
    Opts0 = #etcd_modify_opts{
        key = "/ttlmsg",
        ttl = 2,
        refresh = true
        },
    {"ttl=2", "/ttlmsg?refresh=true"} = etcd_worker:generate_modify_url_and_data_from_opts(Opts0),
    Opts1 = #etcd_modify_opts{
        key = "/prev_value", prev_value = "1",
        value = "2"},
    {"value=2", "/prev_value?prevValue=1"} = etcd_worker:generate_modify_url_and_data_from_opts(Opts1),
    ok.

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
    timer:sleep(600),
    {ok, _} = get_one_node_value("/ttlmsg"),
    etcd:delete("/ttlmsg").

refresh_ttl_only(_) ->
    {ok,_ } = etcd:set("/ttlmsg", "1", 1),
    timer:sleep(500),
    Opts = #etcd_modify_opts{
        key = "/ttlmsg",
        ttl = 2,
        refresh = true
        },
    {ok,_ } = etcd:set(Opts),
    timer:sleep(600),
    {ok, _} = get_one_node_value("/ttlmsg"),
    timer:sleep(2000),
    {fail, not_found} = etcd:get("/ttlmsg"),
    ok.

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
