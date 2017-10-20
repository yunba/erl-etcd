-module(etcd).

-export([
    set/3, set/2, set/1, get/1, delete/1, list_dir/1,
    create_with_auto_increase_key/1,
    watch/2, watch_dir/2, stop_watch/1,
    get_current_peer/0
]).
-include("etcd.hrl").

%%%% set up a key with value with a TTL value(in seconds).
%%%% return is {ok, response string list from etcd}
-spec set(Key::list(), Value::list(), TTL::integer()) -> {ok, list()}| {fail, Reason::atom()}.
set(Key, Value, TTL) ->
    Opt = #etcd_modify_opts{key = Key, value = Value, ttl = TTL},
    Peer = get_current_peer(),
    etcd_worker:etcd_action(set, Peer ++ "/v2", Opt).

%%%% set up a key with value WITHOUT a TTL value.
%%%% return is {ok, response string list from etcd}
-spec set(Key::list(), Value::list()) -> {ok, list()} | {fail, Reason::atom()}.
set(Key, Value ) ->
    Opt = #etcd_modify_opts{key = Key, value = Value},
    Peer = get_current_peer(),
    etcd_worker:etcd_action(set, Peer ++ "/v2", Opt).

%%%% MasterMode, allow all parameters
%%%% return is {ok, response string list from etcd}
-spec set(Opts::#etcd_modify_opts{}) -> {ok, list()}| {fail, Reason::atom()}.
set(Opts) ->
    Peer = get_current_peer(),
    etcd_worker:etcd_action(set, Peer ++ "/v2", Opts).

%%%% MasterMode, allow all parameters
%%%% return is {ok, response string list from etcd}
create_with_auto_increase_key(Opts) ->
    Peer = get_current_peer(),
    etcd_worker:etcd_action(create, Peer ++ "/v2", Opts).


%%%% get the value of a key/dir or just input with an etcd_read_opts.
%%%% return is {ok, response string list from etcd}
%%%% if the key doesn't exist, return {fail, not_found}
-spec get(KeyOrOpts::list() | #etcd_read_opts{}) -> {ok, list()}| {fail, Reason::atom()}.
get(KeyOrOpts) ->
    Opts = case is_record(KeyOrOpts, etcd_read_opts) of
        true ->
            KeyOrOpts;
        false ->
            #etcd_read_opts{key = KeyOrOpts}
    end,
    Peer = get_current_peer(),
    etcd_worker:etcd_action(get, Peer ++ "/v2", Opts).

%%%% get the value of a key/dir or just input with an etcd_read_opts.
%%%% return is {ok, list of nodes under the dir} if success
%%%% if the key doesn't exist, return {fail, not_found}
%%%% if the key is not dir, reutrn {fail, not_dir}

%%% list of nodes will in format : [ PropListOfEtcdRetrunedNode ]
-spec list_dir(KeyOrOpts::list() | #etcd_read_opts{}) -> {ok, list()}| {fail, Reason::atom()}.
list_dir(KeyOrOpts) ->
    Opts = case is_record(KeyOrOpts, etcd_read_opts) of
        true ->
            KeyOrOpts;
        false ->
            #etcd_read_opts{key = KeyOrOpts}
    end,
    Peer = get_current_peer(),
    case  etcd_worker:etcd_action(get, Peer ++ "/v2", Opts) of
        {ok, GetResult} ->
            case jiffy:decode(GetResult) of
                {RetPropList} ->
                    {NodeProp} = proplists:get_value(<<"node">>, RetPropList),
                    IsDir = proplists:get_value(<<"dir">>, NodeProp, false),
                    case IsDir of
                        true ->
                            Nodes = proplists:get_value(<<"nodes">>, NodeProp),
                            RetrivedNodes = [Node || {Node} <- Nodes],
                            {ok, RetrivedNodes};
                        false ->
                            {fail, not_dir}
                    end
            end;
        _ ->
            {fail, not_found}
    end.

%%%% delete the value of a key/dir or just input with an etcd_modify_opts.
%%%% return is {ok, response string list from etcd}
%%%% if the key doesn't exist, it will return {ok, _} as well.
-spec delete(KeyOrOpts::list() | #etcd_modify_opts{}) -> {ok, list()}| {fail, Reason::atom()}.
delete(KeyOrOpts) ->
    Opts = case is_record(KeyOrOpts, etcd_modify_opts) of
        true ->
            KeyOrOpts;
        false ->
            #etcd_modify_opts{key = KeyOrOpts}
    end,
    Peer = get_current_peer(),
    etcd_worker:etcd_action(delete, Peer ++ "/v2", Opts).

%%% wait for the key changing event asynchronously
%%% when the key is changed, Callback function will be called,
%%% and the input will be the response string from etcd.
%%% the Callback should return ok to continue waiting, or stop to exit the waiting.
%%% Alarm: This API won't work for dir
-spec watch(KeyOrOpts::list() | #etcd_read_opts{}, Callback::fun((list())->(ok|stop))) -> {ok, (pid()|undefined)} | {error, atom()}.
watch(KeyOrOpts, Callback) ->
    Opts = case is_record(KeyOrOpts, etcd_read_opts) of
        true -> KeyOrOpts;
        false ->
            #etcd_read_opts{key = KeyOrOpts, modified_index = undefined}
    end,
    gen_server:call(etcd_worker, {watch, Opts, Callback}).

%%% Wait for the dir changing event asynchronously.
%%% A pid is returned for termiating
%%% when the any key in the dir is changed, Callback function will be called,
%%% and the input will be the response string from etcd.
%%% the Callback should return ok to continue waiting, or stop to exit the waiting.
-spec watch_dir(KeyOrOpts::list()| #etcd_read_opts{}, Callback::fun((list())->(ok|stop))) -> {ok, (pid()|undefined)} | {error, atom()}.
watch_dir(KeyOrOpts, Callback) ->
    Opts = case is_record(KeyOrOpts, etcd_read_opts) of
        true -> KeyOrOpts;
        false ->
            #etcd_read_opts{key = KeyOrOpts, modified_index = undefined, recursive = true}
           end,
    gen_server:call(etcd_worker, {watch, Opts, Callback}).

%%% stop watching
-spec stop_watch(Pid::pid()) -> ok|{error, term()}.
stop_watch(Pid) ->
    etcd_sup:stop_child(Pid).

get_current_peer() ->
    Ret = gen_server:call(etcd_worker, {peer}),
    case is_list(Ret) of
        true ->
            Ret;
        _ -> ""
    end.

