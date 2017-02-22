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
        {set, Key, Value, TTL} ->
            etcd_action(set, UrlForV2, Key, Value, integer_to_list(TTL));
        {set, Key, Value} ->
            etcd_action(set, UrlForV2, Key, Value, "");
        {get, Key} ->
            etcd_action(get, UrlForV2, Key);
        {delete, Key} ->
            etcd_action(delete, UrlForV2, Key)
    end,

    {reply, Reply, State}.


handle_cast(Msg, State) ->
    UrlForV2 = proplists:get_value(peer,State) ++ "/v2", 
    case Msg of
        {watch, Key, Callback} ->
            %% TODO: upgrade to gen_fsm and mount it on a supervisor
            spawn(?MODULE, etcd_action, [watch, UrlForV2, Key, Callback]),
            ok;
        _ ->
            ok
    end,
    {noreply, State}.

handle_info(Info, State) ->
    case Info of
        peer_down ->
            body
    end,
    {noreply, State}.

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


etcd_action(set, Url, Key, Value, TTL) ->
    Header = [{"Content-Type", "application/x-www-form-urlencoded"}],
    TTLStr = case TTL of
        "" -> "";
       _ -> "&ttl=" ++ TTL
    end,
    Body = "value=" ++ Value ++ TTLStr, 
    case ibrowse:send_req(Url ++ "/keys" ++ Key, Header, put, Body, [], 5000) of
        {ok, ReturnCode, _Headers, RetBody} ->
            case ReturnCode of
                "200" -> 
                    {ok, RetBody};
                "201" -> 
                    {ok, RetBody};
                _ -> 
                    {fail, wrong_response_code}
            end;
        _Error ->
            {fail, no_response}
    end.

etcd_action(watch, Url, Key, Callback) ->
    case ibrowse:send_req(Url ++ "/keys" ++ Key ++ "?wait=true", [], get, [], [], 5000) of
        {ok, ReturnCode, _Headers, Body} when ReturnCode == "200"->
            BinBody = list_to_binary(Body),
            case jiffy:decode(BinBody) of
                {Props} ->
                    {ok, Callback(Props)};
                _ ->
                    etcd_action(watch, Url, Key, Callback)
            end;
        _ ->
            etcd_action(watch, Url, Key, Callback)
    end.

etcd_action(get, Url, Key) ->
    case ibrowse:send_req(Url ++ "/keys" ++ Key, [], get, [], [], 5000) of
        {ok, ReturnCode, _Headers, Body} ->
            case ReturnCode of
                "200" -> 
                    case jiffy:decode(Body) of
                        {Props} ->
                            %% no error, return value
                            {NodeValue} = proplists:get_value(<<"node">>, Props),
                            Value = proplists:get_value(<<"value">>, NodeValue),
                            {ok, Value};
                        _ ->
                            {fail, wrong_json_body}
                    end;
                "404" ->
                    {fail, not_found};
                _ -> 
                    {fail, wrong_response_code}
            end;
        _ ->
            {fail, no_response}
    end;
etcd_action(delete,Url, Key) ->
    case ibrowse:send_req(Url ++ "/keys" ++ Key, [], delete, [], [], 5000) of
        {ok, ReturnCode, _Headers, Body} ->
            case ReturnCode of
                "200" -> 
                    {ok, Body};
                "404" -> 
                    {ok, Body};
                _ -> 
                    {fail, wrong_response_code}
            end;
        _ ->
            {fail, no_response}
    end.
