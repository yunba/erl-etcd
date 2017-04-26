-module(cache_with_ets_test).

-compile(export_all).
-include("etcd.hrl").

all() ->
    [
        set_up_clean_dir,
        add_key,
        delete_key,
        delete_whole_prefix
    ].

set_up_clean_dir(_) ->
    ets:new(cache_with_ets_test, [named_table, public]),
    {ok, PID} = etcd_ets_cache:watch_prefix(
        "/test_dir2/inner_dir",
        fun(Key, Old, New) ->
            Key = "/test_dir2/inner_dir/case1",
            Old = undefined,
            New = "hello",
            ets:insert(cache_with_ets_test, {set_up_clean_dir, ok})
        end),
    etcd:set("/test_dir2/inner_dir/case1", "hello"),
    timer:sleep(500),

    ct:log("PID is ~p", [PID]),
    etcd:stop_watch(PID),
    case ets:lookup(cache_with_ets_test, set_up_clean_dir) of
        [{set_up_clean_dir, ok}] -> ok;
        Else -> 
            ct:log("other value:~p", [Else]),
            {fail, wrong_value}
    end.

add_key(_) ->
    ets:new(cache_with_ets_test, [named_table, public]),
    {ok, PID} = etcd_ets_cache:watch_prefix(
        "/test_dir2/inner_dir",
        fun(Key, Old, New) ->
            Key = "/test_dir2/inner_dir/case1",
            Old = "hello",
            New = "hello2",
            ets:insert(cache_with_ets_test, {add_key, ok})
        end),
    "hello1" == etcd_ets_cache:get("/test_dir2/inner_dir/case"),

    etcd:set("/test_dir2/inner_dir/case1", "hello2"),
    timer:sleep(500),

    "hello2" == etcd_ets_cache:get("/test_dir2/inner_dir/case"),

    etcd:stop_watch(PID),
    case ets:lookup(cache_with_ets_test, add_key) of
        [{add_key, ok}] -> ok;
        Else -> 
            ct:log("other value:~p", [Else]),
            {fail, wrong_value}
    end.

delete_key(_) ->
    ets:new(cache_with_ets_test, [named_table, public]),
    {ok, PID} = etcd_ets_cache:watch_prefix(
        "/test_dir2/inner_dir",
        fun(Key, Old, New) ->
            Key = "/test_dir2/inner_dir/case1",
            Old = "hello2",
            New = undefined,
            ets:insert(cache_with_ets_test, {delete_key, ok})
        end),
    etcd:delete("/test_dir2/inner_dir/case1"),
    timer:sleep(500),

    etcd:stop_watch(PID),
    case ets:lookup(cache_with_ets_test, delete_key) of
        [{delete_key, ok}] -> ok;
        Else -> 
            ct:log("other value:~p", [Else]),
            {fail, wrong_value}
    end.

delete_whole_prefix(_) -> 
    ets:new(cache_with_ets_test, [named_table, public]),
    {ok, PID} = etcd_ets_cache:watch_prefix(
        "/test_dir2/inner_dir",
        fun(Key, Old, New) ->
            Key = "/test_dir2",
            Old = undefined,
            New = undefined,
            ets:insert(cache_with_ets_test, {delete_whole_prefix, ok})
        end),
    etcd:delete("/test_dir2"),
    timer:sleep(500),

    etcd:stop_watch(PID),
    case ets:lookup(cache_with_ets_test, delete_whole_prefix) of
        [{delete_whole_prefix, ok}] -> ok;
        Else -> 
            ct:log("other value:~p", [Else]),
            {fail, wrong_value}
    end.
