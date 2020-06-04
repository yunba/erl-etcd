-module(json_kv_test).

-compile(export_all).
-include("etcd.hrl").

all() ->
    [
        clean,
        init_string,
        merge_string,
        replace_string,
        get_string
%        clean
    ].

init_per_group(_, Config) -> Config.
end_per_group(_GroupName, _Config) -> ok.

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

get_string(_) ->
    Ret = etcd_json_kv:get_as_string("/test_json"),
    ct:log("ret is ~p", [Ret]),
    {DecodedObj} = jiffy:decode(Ret),
    <<"test">> = proplists:get_value(<<"hello">>, DecodedObj),
    {ObjBody} = proplists:get_value(<<"obj">>, DecodedObj),
    <<"hello1">> = proplists:get_value(<<"objkey1">>, ObjBody),

    ListBody = proplists:get_value(<<"list">>, DecodedObj),
    [{ObjInArray}, <<"3">>, <<"4">>, <<"5">>] = ListBody,
    [{<<"hello">>, <<"in array">>}] = ObjInArray,

    ok.
