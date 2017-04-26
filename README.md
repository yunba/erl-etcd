## erl-etcd

Erlang driver for etcd. Requried R16 or higher, basing on etcd v2 http APIs.

## supporting feature
- basic etcd api (etcd module)
- watch a dir and cache all changed value in this dir into ets (etcd_ets_cache module)
- get and set json value into etcd keys (etcd_json_kv module)

## Quick start

Config etcd address:

```erlang
{etcd, [{addr, ["http://address1:port1", "http://address2:port2", "http://address3:port3"]}]
```

Start the application and do some basic action:

```erlang
%% start the application
application:start(etcd).

%% set a key
etcd:set("/message", "hello").

%% get a key/dir
{ok, Body} = etcd:get("/message").

%% delete a key/dir
etcd:delete("/message").

%% watch a key
Callback = fun(V) -> io:format(V), ok end,
etcd:watch("/message", Callback).

%% watch a dir
Callback = fun(V) -> io:format(V), ok end,
etcd:watch_dir("/test_dir", Callback).


```

Refer to `src/etcd.erl` to see more details about APIs.

## using json kv feature

consider the following json object:

```
{
"key":"value",
"obj": {"obj_key":"obj_value"},
"list": [1,2,3, {"obj_list":"list_value"}]
}
```

make this a string into erlang , and call the following api:

```
etcd_json_kv:set_from_string("/json_test_prefix", JsonString).
```

And then you can get json value in etcd:

```
/json_test_prefix/key -> "value"
/json_test_prefix/obj/obj_key -> "obj_value"
/json_test_prefix/list/0 -> "1"
/json_test_prefix/list/1 -> "2"
/json_test_prefix/list/2 -> "3"
/json_test_prefix/list/4/obj_list -> "list_value"

```
So you can get them as native etcd get request.

To get the json string back , you can call the following API:

```
etcd_json_kv:get_as_string("/json_test_prefix")
```

Refer to `src/etcd_json_kv.erl` for more details.

## using ets as cache and watch for change

Try this:

```
etcd_ets_cache:watch_prefix(
        "/test_dir",
        fun(Key, OldValue, NewValue) ->
        		io:format("~p", [{Key, OldValue, NewValue}]),
        		ok
        end).
```

This will watch on the prefix of `/test_dir`, any changed on this dir will be caught and `[Key, OldValue, NewValue]` will be passed to the callback. And the newest value is stored into `ets`, and you can access them by calling

```
etcd_ets_cache:get("/test_dir/keyx").
```

refer to `src/etcd_ets_cache.erl` for more details

## v0.2-beta
- add json_kv
- integer ets cache with etcd watch
- bug fix for `space` and `plus` url encoding 

## v0.1-beta
- All wait behaviour will be monitored by etcd_sup
- Add almost all opt support

## v0.1-alpha
This is the first alpha version of etcd erlang client. Supporting:
- Basic CURD
- Basic wait operation


