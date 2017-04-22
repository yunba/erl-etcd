-module(etcd_ets_cache).

-include("etcd.hrl").

%% this module will provide you a ets wrapped api, that can cache value with ets,
%% and wait for change on a prefix


%%% Wait for the dir changing event asynchronously.
%%% A pid is returned for termiating
%%% when the any key in the prefix is changed, Callback function will be called,
%%% and the input will be the response string from etcd.
%%% the Callback should return ok to continue waiting, or stop to exit the waiting.
-spec watch_prefix(KeyOrOpts::list()| #etcd_read_opts{}, Callback::fun((OldValue::list(), NewValue::list())->(ok|stop))) -> {ok, (pid()|undefined)} | {error, atom()}.
watch_prefix(Prefix, Callback) ->
    etcd:watch(Prefix, fun(Body) ->
        {JsonBody} = jiffy:decode(Body),
        NewValue = proplists:get_value(<<"node">>, JsonBody, undefined),
        OldValue = proplists:get_value(<<"prevNode">>, JsonBody, undefined),
        Key = proplists:get_value(<<"key">>, OldValue),
        ets:insert(etcd_ets_cache, Key, NewValue),
        Callback(OldValue, NewValue)
    end).

find_key(Key) ->
    case ets:lookup(etcd_ets_cache, Key) of
        [{_, Value}] when undefined =/= Value -> Value;
        _ -> 
            case etcd:get(Key) of
                {ok, Body} ->
                    {JsonBody} = jiffy:decode(Body),
                    NewValue = proplists:get_value(<<"node">>, JsonBody, undefined),
                    ets:insert(Key, NewValue),
                    {ok, NewValue};
                _ ->
                    {fail, not_found}
            end
    end.

