## erl-etcd

Erlang driver for etcd. Requried R16 or higher

## Quick start

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

## v0.1-alpha
This is the first alpha version of etcd erlang client. Supporting:
- Basic CURD
- Basic wait operation
