-module(json_kv_test).

-compile(export_all).
-include("etcd.hrl").

all() ->
    [
        clean,
        init_string,
        merge_string,
        replace_string,
        clean
    ].

clean(_) ->
    etcd:delete("/test_json").

init_string(_) ->
    etcd_json_kv:set_from_string("/test_json",
        <<"{\"hello\":\"test\",\"obj\":{\"objkey1\":\"hello1\"},\"list\":[{\"hello\":\"in array\"},3,4,5]}">>),
    {ok, <<"test">>} = etcd_SUITE:get_one_node_value("/test_json/hello"),
    {ok, <<"hello1">>} = etcd_SUITE:get_one_node_value("/test_json/obj/objkey1"),
    {ok, <<"in array">>} = etcd_SUITE:get_one_node_value("/test_json/list/0/hello"),
    {ok, <<"3">>} = etcd_SUITE:get_one_node_value("/test_json/list/1"),
    {ok, <<"4">>} = etcd_SUITE:get_one_node_value("/test_json/list/2"),
    {ok, <<"5">>} = etcd_SUITE:get_one_node_value("/test_json/list/3").

merge_string(_) ->
    etcd_json_kv:set_from_string("/test_json",
        <<"{\"hello\":\"test\",\"obj\":{\"objkey1\":\"hello1\"},\"list\":[{\"hello\":\"in array\"},3,4,5]}">>),

    {ok, <<"test">>} = etcd_SUITE:get_one_node_value("/test_json/hello"),
    {ok, <<"hello1">>} = etcd_SUITE:get_one_node_value("/test_json/obj/objkey1"),
    {ok, <<"in array">>} = etcd_SUITE:get_one_node_value("/test_json/list/4/hello"),
    {ok, <<"3">>} = etcd_SUITE:get_one_node_value("/test_json/list/5"),
    {ok, <<"4">>} = etcd_SUITE:get_one_node_value("/test_json/list/6"),
    {ok, <<"5">>} = etcd_SUITE:get_one_node_value("/test_json/list/7").

replace_string(_) ->
    etcd_json_kv:replace_from_string("/test_json",
        <<"{\"hello\":\"test\",\"obj\":{\"objkey1\":\"hello1\"},\"list\":[{\"hello\":\"in array\"},3,4,5]}">>),
    {ok, <<"test">>} = etcd_SUITE:get_one_node_value("/test_json/hello"),
    {ok, <<"hello1">>} = etcd_SUITE:get_one_node_value("/test_json/obj/objkey1"),
    {ok, <<"in array">>} = etcd_SUITE:get_one_node_value("/test_json/list/0/hello"),
    {ok, <<"3">>} = etcd_SUITE:get_one_node_value("/test_json/list/1"),
    {ok, <<"4">>} = etcd_SUITE:get_one_node_value("/test_json/list/2").
