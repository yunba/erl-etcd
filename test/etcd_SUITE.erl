%% all etcd keys you used here should start with /testing_entry, so you can delete it with one delete command in the init_per_suite


-module(etcd_SUITE).

-compile(export_all).
-include("etcd.hrl").


all() ->
    [
        gen_read_opt_querys,
        gen_write_opt_data_and_querys,
        set_value,
        create_with_auto_increase_key,
        set_value_with_ttl,
        set_get_value_with_plus,
        refresh_value_with_ttl,
        refresh_ttl_only,
        only_work_when_prev_exist,
        only_work_when_prev_value,
        only_work_when_prev_index,
        get_value,
        watch_value,
        watch_dir,
        get_dir,
        list_dir,
        get_other_peer_if_current_one_is_not_alive,
        {group, json_kv}
    ].

groups() ->
    [
        {json_kv, [], [{json_kv_test, all}]}
    ].


only_work_when_prev_index(_) ->
    etcd:delete("/testing_entry/prev_index"),
    {ok, Body} = etcd:set("/testing_entry/prev_exist", "1"),
    %%TODO
    ok.

only_work_when_prev_exist(_) ->
    etcd:delete("/testing_entry/prev_exist"),
    etcd:delete("/testing_entry/prev_exist2"),
    etcd:set("/testing_entry/prev_exist", "1"),
    Opts1 = #etcd_modify_opts{
        key = "/testing_entry/prev_exist", prev_exist = true,
        value = "2"},
    etcd:set(Opts1),
    {ok, <<"2">>} = get_one_node_value("/testing_entry/prev_exist"),
    Opts2 = #etcd_modify_opts{
        key = "/testing_entry/prev_exist2", prev_exist = true,
        value = "2"},
    etcd:set(Opts2),
    {fail, not_found} = etcd:get("/testing_entry/prev_exist2"),
    etcd:delete("/testing_entry/prev_exist"),
    ok.

only_work_when_prev_value(_) ->
    etcd:delete("/testing_entry/prev_value"),
    etcd:set("/testing_entry/prev_value", "1"),
    {ok, <<"1">>} = get_one_node_value("/testing_entry/prev_value"),
    Opts1 = #etcd_modify_opts{
        key = "/testing_entry/prev_value", prev_value = "1",
        value = "2"},
    etcd:set(Opts1),
    {ok, <<"2">>} = get_one_node_value("/testing_entry/prev_value"),
    
    Opts2 = #etcd_modify_opts{
        key = "/testing_entry/prev_value", prev_value = "1",
        value = "3"},
    etcd:set(Opts2),
    {ok, <<"2">>} = get_one_node_value("/testing_entry/prev_value"),
    ok.

create_with_auto_increase_key(_) ->
    Opts1 = #etcd_modify_opts{
        key = "/testing_entry/increase_key",
        value = "2"},
    etcd:create_with_auto_increase_key(Opts1),
    {ok, _} = etcd:get("/testing_entry/increase_key").


init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(etcd),
    etcd:delete("/testing_entry"),
    Config.

end_per_suite(_Config) ->
    ok.

gen_read_opt_querys(_) ->
    ok.

gen_write_opt_data_and_querys(_) ->
    Opts0 = #etcd_modify_opts{
        key = "/testing_entry/ttlmsg",
        ttl = 2,
        refresh = true
    },
    {{form, [{"ttl", 2}]}, "/testing_entry/ttlmsg?refresh=true"} = etcd_worker:generate_modify_url_and_data_from_opts(Opts0),
    Opts1 = #etcd_modify_opts{
        key = "/testing_entry/prev_value", prev_value = "1",
        value = "2"},
    {{form, [{"value", "2"}]}, "/testing_entry/prev_value?prevValue=1"} = etcd_worker:generate_modify_url_and_data_from_opts(Opts1),
    ok.

set_value(_) ->
    {ok, _} = etcd:set("/testing_entry/message", "1").

set_value_with_ttl(_) ->
    {ok, _} = etcd:set("/testing_entry/ttlmsg", "1", 1),
    timer:sleep(2000),
    {fail, not_found} = etcd:get("/testing_entry/ttlmsg"),
    ok.
set_get_value_with_plus(_) ->
    {ok, _} = etcd:set("/testing_entry/url_encode_msg+123 ", "1234 56+1234 56+"),
    {ok, <<"1234 56+1234 56+">>} = get_one_node_value("/testing_entry/url_encode_msg+123 "),
    ok.

refresh_value_with_ttl(_) ->
    {ok, _} = etcd:set("/testing_entry/ttlmsg", "1", 1),
    timer:sleep(500),
    {ok, _} = etcd:set("/testing_entry/ttlmsg", "", 2),
    timer:sleep(600),
    {ok, _} = get_one_node_value("/testing_entry/ttlmsg"),
    etcd:delete("/testing_entry/ttlmsg").

refresh_ttl_only(_) ->
    {ok, _} = etcd:set("/testing_entry/ttlmsg", "1", 1),
    timer:sleep(500),
    Opts = #etcd_modify_opts{
        key = "/testing_entry/ttlmsg",
        ttl = 2,
        refresh = true
    },
    {ok, _} = etcd:set(Opts),
    timer:sleep(600),
    {ok, _} = get_one_node_value("/testing_entry/ttlmsg"),
    timer:sleep(2000),
    {fail, not_found} = etcd:get("/testing_entry/ttlmsg"),
    ok.

get_value(_) ->
    {ok, <<"1">>} = get_one_node_value("/testing_entry/message").

watch_value(_) ->
    %% this test is a little care about timing...
    Callback = self(),
    {ok, _} = etcd:watch("/testing_entry/message", Callback, test),
    timer:sleep(300),
    
    {ok, Msg} = etcd:set("/testing_entry/message", "2"),
    ct:log("msg is :~p", [Msg]),
    
    Res1 = receive {test, #{}} = Event ->
        ct:log("receive Event:~p~n", [Event]),
        ok
           after 2000 ->
            {error, timeout}
           end,
    Res1 = ok,
    {ok, DelMsg} = etcd:delete("/testing_entry/message"),
    ct:log("msg is :~p", [DelMsg]),
    Res2 = receive {test, #{}} = Event1 ->
        ct:log("receive Event:~p~n", [Event1]),
        ok
           after 2000 ->
            {error, timeout}
           end,
    Res2 = ok.

watch_dir(_) ->
    CallBack = self(),
    {ok, WPid} = etcd:watch_dir("/testing_entry/test_dir", CallBack, test1),
    timer:sleep(300),
    {ok, _} = etcd:set("/testing_entry/test_dir/hello", "2"),
    Res1 = receive {test1, #{}} = Event1 ->
        ct:log("receive Event:~p~n", [Event1]),
        ok
           after 2000 ->
            {error, timeout}
           end,
    Res1 = ok,
    {ok, _} = etcd:set("/testing_entry/test_dir/hello2", "2"),
    Res2 = receive {test1, #{}} = Event2 ->
        ct:log("receive Event:~p~n", [Event2]),
        ok
           after 2000 ->
            {error, timeout}
           end,
    Res2 = ok,
    
    etcd:stop_watch(WPid),
    etcd:delete("/testing_entry/dir_test"),
    Result = receive _ ->
        error
             after 1500 ->
            ok
             end,
    ok = Result,
    ok.

get_dir(_) ->
    {ok, Body} = etcd:get("/testing_entry/test_dir"),
    ct:log("the body is :~p", [Body]),
    ok.

list_dir(_) ->
    {ok, Body} = etcd:list_dir("/testing_entry/test_dir"),
    ct:log("the body is :~p", [Body]),
    ok.

get_one_node_value(Key) ->
    {ok, Body} = etcd:get(Key),
    ct:log("body is :~p", [Body]),
    get_value_from_response_body(Body).

get_value_from_response_body(Body) ->
    case jiffy:decode(Body) of
        {Props} ->
            %% no error, return value
            {NodeValue} = proplists:get_value(<<"node">>, Props),
            Value = proplists:get_value(<<"value">>, NodeValue),
            ct:log("node value is :~p", [NodeValue]),
            {ok, Value};
        _ ->
            {fail, wrong_json_body}
    end.

get_other_peer_if_current_one_is_not_alive(_) ->
    Opts1 = #etcd_modify_opts{
        key = "/testing_entry/message",
        value = "2"},
    Opts2 = #etcd_read_opts{
        key = "/testing_entry/message"},
    Opts3 = #etcd_modify_opts{
        key = "/testing_entry/message"
    },
    {ok, _} = etcd_worker:etcd_action(set, "http://localhost:1111/v2/keys", Opts1),
    {ok, _} = etcd_worker:etcd_action(get, "http://localhost:1111/v2/keys", Opts2),
    {ok, _} = etcd_worker:etcd_action(delete, "http://localhost:1111/v2/keys", Opts3).
