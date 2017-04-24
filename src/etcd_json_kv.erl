%% set up a value by json and get the values by "prefix" + json key
%% setting {"case1":"hello"} to "/test_key" whill result in:
%% "/test_key/case1" => "hello"

-module(etcd_json_kv).
-export([set/2, set_from_string/2]).
-export([replace/2, replace_from_string/2]).

%% as it's hard to tell if a "ABC" is a string or an array [65, 66, 67] , so I treat them as a list here,
%% hence if you wanna set a value of string, you should input a binary, 

replace(Prefix, Content) -> 
    etcd:delete(Prefix),
    set(Prefix, Content).

replace_from_string(Prefix, String) ->
    etcd:delete(Prefix),
    set_from_string(Prefix, String).
get_max_index_key(Key) ->
    Key ++ "/max_index".

set(Prefix, {Key, {ObjValue}}) ->
    NextKey = Prefix ++ "/" ++ etcd_util:make_sure_list(Key),
    set(NextKey, ObjValue);
set(Prefix, {Key, ListValue}) when is_list(ListValue)->
    KeyPrefix = Prefix ++ "/" ++ etcd_util:make_sure_list(Key),
    FreeIndex = get_free_index_no(KeyPrefix),
    LastIndex = lists:foldl(fun(CurValue, Counter) ->
        CurKey = KeyPrefix ++ "/" ++ etcd_util:make_sure_list(Counter),
        set(CurKey, CurValue),
        Counter + 1
    end, FreeIndex, ListValue),
    etcd:set(get_max_index_key(KeyPrefix), integer_to_list(LastIndex));
set(Prefix, {Key, Value}) ->
    EtcdKey = Prefix ++ "/" ++ etcd_util:make_sure_list(Key),
    etcd:set(EtcdKey, etcd_util:make_sure_list(Value));
set(Prefix, {ArrayValue}) when is_list(ArrayValue) ->
    lists:foreach(fun(Value) ->
        set(Prefix, Value)
    end, ArrayValue);
set(Prefix, ArrayValue) when is_list(ArrayValue) ->
    lists:foreach(fun(Value) ->
        set(Prefix, Value)
    end, ArrayValue);
set(Prefix, Value) ->
    etcd:set(Prefix, etcd_util:make_sure_list(Value)).

set_from_string(Prefix, JsonString) ->
    JsonStringBin = etcd_util:make_sure_binary(JsonString),
    Body = jiffy:decode(JsonStringBin),
    set(Prefix, Body).

get_free_index_no(Key) ->
    CurKey = get_max_index_key(Key),
    case etcd:get(CurKey) of
        {ok, Body} -> 
            Value = etcd_helper:get_value_from_body(Body),
            binary_to_integer(Value);
        {fail, not_found} -> 
            0;
        _ -> {fail, internal_error}
    end.
