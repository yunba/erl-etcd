-module(etcd_worker).

-behavior(gen_server).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).
-include("etcd.hrl").

%%%===================================================================
%%% API
%%%===================================================================
-export([start_link/1]).

%% export for spawn
-export([etcd_action/4]).

start_link(Args) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Args], []).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([EtcdPeers]) ->
    Peer = get_health_peer(EtcdPeers),
    case Peer of
        undefined ->
            {stop, no_etcd_peer_alive};
        _ ->
            {ok, [{peer, Peer}, {etcds, EtcdPeers}]}
    end.

handle_call(Request, _From, State) ->
    UrlForV2 = proplists:get_value(peer,State) ++ "/v2", 
    Reply = case Request of
        {set, Opts} ->
            etcd_action(set, UrlForV2, Opts);
        {get, Opts} ->
            etcd_action(get, UrlForV2, Opts);
        {delete, Opts} ->
            etcd_action(delete, UrlForV2, Opts)
    end,

    {reply, Reply, State}.


handle_cast(Msg, State) ->
    UrlForV2 = proplists:get_value(peer,State) ++ "/v2", 
    case Msg of
        {watch, Opts, Callback} ->
            %% TODO: upgrade to gen_fsm and mount it on a supervisor
            spawn(?MODULE, etcd_action, [watch, UrlForV2, Opts, Callback]);
        _ ->
            ok
    end,
    {noreply, State}.

handle_info(Info, State) ->
    NextState = case Info of
        peer_down->
            EtcdPeers = proplists:get_value(etcds,State), 
            NewPeer = get_health_peer(EtcdPeers),
            case NewPeer of
                undefined -> 
                    timer:send_after(5000, self(), peer_down),
                    State;
                _ -> 
                    %% update old peer to new peer
                    CleanState = proplists:delete(peer, State),
                    CleanState ++ [{peer, NewPeer}]
            end;
        _ ->
            State
    end,
    {noreply, NextState}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_health_peer(EtcdPeers) ->
    lists:foldl(
        fun(Url, ActivePeer) -> 
            case ActivePeer of
                undefined ->
                    case check_peer_alive(Url) of
                        true -> Url;
                        false -> undefined
                    end;
                _ ->
                    ActivePeer
            end
        end, undefined, EtcdPeers).

check_peer_alive(Url) ->
    case ibrowse:send_req(Url ++ "/version", [], get, [], [], 5000) of
        {ok, ReturnCode, _Headers, _Body} ->
            case ReturnCode of
                "200" -> true;
                _ -> false
            end;
        _ ->
            false
    end.

%% handle all opts
etcd_action(set, Url, Opts) ->
    Header = [{"Content-Type", "application/x-www-form-urlencoded"}],
    TTLStr = case Opts#etcd_modify_opts.ttl of
        undefined -> "";
        "" -> "&ttl=";
        TTL -> "&ttl=" ++ integer_to_list(TTL)
    end,
    Key = Opts#etcd_modify_opts.key,
    Value = Opts#etcd_modify_opts.value,
    Body = "value=" ++ Value ++ TTLStr, 
    case ibrowse:send_req(Url ++ "/keys" ++ Key, Header, put, Body, [], 5000) of
        {ok, ReturnCode, _Headers, RetBody} ->
            case ReturnCode of
                "200" -> 
                    {ok, RetBody};
                "201" -> 
                    {ok, RetBody};
                _ -> 
                    {fail, {wrong_response_code, Body}}
            end;
        {error,{conn_failed,{error,econnrefused}}} ->
            self() ! peer_down,
            {fail, peer_down};
        Reason ->
            {fail, Reason}
    end;
etcd_action(get, Url, Opts) ->
    Key = Opts#etcd_read_opts.key,
    case ibrowse:send_req(Url ++ "/keys" ++ Key, [], get, [], [], 5000) of
        {ok, ReturnCode, _Headers, Body} ->
            case ReturnCode of
                "200" -> 
                    {ok, Body};
                "404" ->
                    {fail, not_found};
                _ -> 
                    {fail, {wrong_response_code, Body}}
            end;
        {error,{conn_failed,{error,econnrefused}}} ->
            self() ! peer_down,
            {fail, peer_down};
        Reason ->
            {fail, Reason}
    end;
etcd_action(delete,Url, Opts) ->
    Key = Opts#etcd_modify_opts.key,
    case ibrowse:send_req(Url ++ "/keys" ++ Key ++ "?recursive=true", [], delete, [], [], 5000) of
        {ok, ReturnCode, _Headers, Body} ->
            case ReturnCode of
                "200" -> 
                    {ok, Body};
                "404" -> 
                    {ok, Body};
                _ -> 
                    {fail, {wrong_response_code, Body}}
            end;
        {error,{conn_failed,{error,econnrefused}}} ->
            self() ! peer_down,
            {fail, peer_down};
        Reason ->
            {fail, Reason}
    end.
%% handle all opts
etcd_action(watch, Url, Opts, Callback) ->
    OptStr = case Opts of
        undefined -> 
            ModifiedIndex = undefined,
            "";
        #etcd_read_opts{
            recursive = Recursive,
            sorted = Sorted,
            modified_index = ModifiedIndex} ->

            RecursiveStr = case Recursive of
                true -> "&recursive=true";
                _ -> ""
            end,
            SortedStr = case Sorted of
                true -> "&sorted=true";
                _ -> ""
            end,
            ModifiedIndexStr = case ModifiedIndex of
                undefined ->
                    "";
                _ ->
                    "&waitIndex=" ++ integer_to_list(ModifiedIndex)
            end,
            RecursiveStr ++ SortedStr ++ ModifiedIndexStr
    end,

    Key = Opts#etcd_read_opts.key,

    case ibrowse:send_req(Url ++ "/keys" ++ Key ++ "?wait=true" ++ OptStr, [], get, [], [], 60000) of
        {ok, ReturnCode, _Headers, Body} when ReturnCode == "200"->
            NewIndex = case get_modified_index_from_response_body(Body) of
                {ok, NewModifiedIndex} -> NewModifiedIndex + 1;
                _ -> ModifiedIndex
            end,
            CallbackRet = Callback(Body),
            NewOpts = Opts#etcd_read_opts{modified_index = NewIndex},
            case CallbackRet of
                ok -> etcd_action(watch, Url, NewOpts, Callback);
                stop -> ok;
                _ -> error
            end;
        {error,{conn_failed,{error,econnrefused}}} ->
            self() ! peer_down,
            etcd_action(watch, Url, Opts, Callback);
        _ ->
            etcd_action(watch, Url, Opts, Callback)
    end.

%% FIXME: what if the body returns a list
get_modified_index_from_response_body(Body) ->
    case jiffy:decode(Body) of
        {Props} ->
            %% no error, return value
            {NodeValue} = proplists:get_value(<<"node">>, Props),
            Value = proplists:get_value(<<"modifiedIndex">>, NodeValue),
            {ok, Value};
        _ ->
            {fail, wrong_json_body}
    end.
