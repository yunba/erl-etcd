%% this module will provide you a ets wrapped api, that can cache value with ets,
%% and wait for change on a prefix

-module(etcd_ets_cache).

-include("etcd.hrl").

-export([get/1, watch_prefix/2]).


%%% Wait for the dir changing event asynchronously.
%%% A pid is returned for termiating
%%% when the any key in the prefix is changed, Callback function will be called,
%%% and the input will be the response string from etcd.
%%% the Callback should return ok to continue waiting, or stop to exit the waiting.
%%
%%
%% Old and New value can be :
%% undefined undefined -> when the whole prefix is deleted
%% undefined OtherTerm -> when a new key is created
%% OtherTerm undefined -> when a key is deleted
%% OtherTerm OtherTerm -> when a value is modified
-spec watch_prefix(KeyOrOpts::list()| #etcd_read_opts{}, Callback::fun((Key::list(), OldValue::list(), NewValue::list())->(ok|stop))) -> {ok, (pid()|undefined)} | {error, atom()}.
watch_prefix(Prefix, Callback) ->
    etcd:watch_dir(Prefix, fun(Body) ->
        {JsonBody} = jiffy:decode(Body),
        NewNodeContent = case proplists:get_value(<<"node">>, JsonBody, undefined) of
                  undefined -> undefined;
                  {NewNode} -> NewNode
                         end,
        OldNodeContent = case proplists:get_value(<<"prevNode">>, JsonBody, undefined) of
                  undefined -> undefined;
                  {OldNode} -> OldNode
                         end,
        Key = case NewNodeContent of
            undefined -> proplists:get_value(<<"key">>, OldNodeContent);
            _ -> proplists:get_value(<<"key">>, NewNodeContent)
              end,
        KeyString = case Key of
            undefined -> undefined;
            _ -> binary_to_list(Key)
        end,
        NewValue = get_value_from_node(NewNodeContent),
        OldValue = get_value_from_node(OldNodeContent),
        ets:insert(etcd_ets_cache, {KeyString, NewValue}),
        Callback(KeyString, OldValue, NewValue)
    end).

-spec get(Key::string()) -> {ok, Value::binary()} | {fail, not_found}.
get(Key) ->
    case ets:lookup(etcd_ets_cache, Key) of
        [{_, Value}] when undefined =/= Value -> 
            {ok, Value};
        _ -> 
            case etcd:get(Key) of
                {ok, Body} ->
                    {JsonBody} = jiffy:decode(Body),
                    {Node} = proplists:get_value(<<"node">>, JsonBody, undefined),
                    NewValue = get_value_from_node(Node),
                    ets:insert(etcd_ets_cache, {Key, NewValue}),
                    {ok, NewValue};
                _ ->
                    {fail, not_found}
            end
    end.

get_value_from_node(Node) ->
    case Node of
        undefined -> undefined;
        _ -> 
            case proplists:get_value(<<"value">>, Node) of
                undefined -> undefined;
                BinValue -> binary_to_list(BinValue)
            end
    end.
