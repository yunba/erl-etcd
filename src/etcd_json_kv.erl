%% set up a value by json and get the values by "prefix" + json key
%% setting {"case1":"hello"} to "/test_key" whill result in:
%% "/test_key/case1" => "hello"

-module(etcd_json_kv).
-export([set/2, set_from_string/2]).
-export([get_as_e_json/1, get_as_string/1]).
-export([replace/2, replace_from_string/2]).

%% as it's hard to tell if a "ABC" is a string or an array [65, 66, 67] , so I treat them as a list here,
%% hence if you wanna set a value of string, you should input a binary, 

%% Cotent should follow the jiffy's ejson format, what is ejson? try to use jiffy:decode and see the output
-spec replace(Prefix::list(), Content::term()) -> {ok, Body::list()}|{error, Reason::term()}.
replace(Prefix, Content) -> 
    etcd:delete(Prefix),
    set(Prefix, Content).

%% Content should be a json string.
-spec replace_from_string(Prefix::list(), Content::list()) -> {ok, Body::list()}|{error, Reason::term()}.
replace_from_string(Prefix, String) ->
    etcd:delete(Prefix),
    set_from_string(Prefix, String).
get_max_index_key(Key) ->
    %% should change to __max_index
    Key ++ "/max_index".

%% EJsonContent can be any type of ejson content
-spec set(Prefix::list(), EJsonContent::term()) -> {ok, Body::list()} | {error, Reason::term()}.
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

-spec set_from_string(Prefix::list(), JsonString::list()) -> {ok, Body::list()} | {error, Reason::term()}.
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

get_as_e_json(Prefix, -1, Output) ->
    Output;
get_as_e_json(Prefix, CurrentIndex, Output) when is_integer(CurrentIndex) ->
    CurKey = Prefix ++ etcd_util:make_sure_list(CurrentIndex),
    NextOutput = case get_as_e_json(CurKey) of
        undefined -> Output;
        GetObj -> [GetObj] ++ Output
    end,
    get_as_e_json(Prefix, CurrentIndex - 1, NextOutput).

get_as_e_json(Prefix, Key) ->
    NewPrefix = Prefix ++ etcd_util:make_sure_list(Key),
    {IsArray, MaxIndex} = case etcd:get(get_max_index_key(NewPrefix)) of
        {ok, Body} ->
            MaxIndexBin = etcd_helper:get_value_from_body(Body),
            {true, binary_to_integer(MaxIndexBin)};
        {fail, _} ->
            {false, undefined}
    end,
    case IsArray of
        true ->
            get_as_e_json(NewPrefix ++ "/", MaxIndex, []);
        false ->
            get_as_e_json(NewPrefix)
    end.

-spec get_as_e_json(Prefix::list()) -> {EJsonBody::term()} | undefined.
get_as_e_json(Prefix) ->
    case etcd:get(Prefix) of
        {ok, Body} ->
            {JsonBody} = jiffy:decode(Body),
            {Node} = proplists:get_value(<<"node">>, JsonBody),
            case proplists:get_value(<<"dir">>, Node, false) of
                false -> 
                    Value = proplists:get_value(<<"value">>, Node),
                    Value;
                true ->
                    Nodes = proplists:get_value(<<"nodes">>, Node),
                    RetList = lists:foldl(fun({OneNode}, CurList) ->
                        Key = proplists:get_value(<<"key">>, OneNode),
                        JsonKey = binary:replace(Key, etcd_util:make_sure_binary(Prefix ++ "/"), <<"">>),
                        JsonValue = get_as_e_json(Prefix ++ "/", JsonKey),
                        CurList ++ [{JsonKey, JsonValue}]
                    end, [], Nodes),
                    {RetList}
            end;
        _ ->
            undefined
    end.

-spec get_as_string(Prefix::list()) -> {JsonString::list()} | error.
get_as_string(Prefix) ->
    EJson = get_as_e_json(Prefix),
    jiffy:encode(EJson).

