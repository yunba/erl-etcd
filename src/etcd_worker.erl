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

-export([generate_modify_url_and_data_from_opts/1, generate_read_str_from_opts/1]).
-include("etcd.hrl").

%%%===================================================================
%%% API
%%%===================================================================
-export([start_link/1]).
-export([etcd_action/3]).

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
    Peer = proplists:get_value(peer,State),
    Reply = case Request of
        {peer_down} ->
            EtcdPeers = proplists:get_value(etcds,State),
            NewPeer = get_health_peer(EtcdPeers),
            case NewPeer of
                undefined ->
                    erlang:send_after(5000, self(), peer_down);
                _ ->
                    NewPeer
            end;
        {peer} ->
            Peer;
        {watch, Opts, Callback} ->
            ChildSpec = {
                {Opts, Callback},   % use {opts, callback} as id
                {etcd_watch_behaviour, start_watch, [Opts, Callback]},
                transient, 5000,
                worker, [etcd_watch_behaviour, ?MODULE]},
            etcd_sup:add_child(ChildSpec);
        _ ->
            ok
    end,
    NewState = case Request of
        {peer_down} when reply =/= undefined ->
            CleanState = proplists:delete(peer, State),
            CleanState ++ [{peer, Reply}];
        _ ->
            State
    end,

    {reply, Reply, NewState}.


handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(Info, State) ->
    NextState = case Info of
        peer_down->
            EtcdPeers = proplists:get_value(etcds,State), 
            NewPeer = get_health_peer(EtcdPeers),
            case NewPeer of
                undefined -> 
                    %% no peer is on, wait 5s and re-send
                    erlang:send_after(5000, self(), peer_down),
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

etcd_action(create, V2Url, Opts) ->
    Header = [{"Content-Type", "application/x-www-form-urlencoded"}],
    {Body, QueryStr} = generate_modify_url_and_data_from_opts(Opts), 
    case ibrowse:send_req(V2Url ++ "/keys" ++ QueryStr, Header, post, Body, [], 5000) of
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
            case gen_server:call(?MODULE, {peer_down}) of
                undefined ->
                    {fail, peer_down};
                NewPeer ->
                    etcd_action(post, NewPeer ++ "/v2/keys", Opts)
            end;
        Reason ->
            {fail, Reason}
    end;
etcd_action(set, V2Url, Opts) ->
    Header = [{"Content-Type", "application/x-www-form-urlencoded"}],
    {Body, QueryStr} = generate_modify_url_and_data_from_opts(Opts), 
    case ibrowse:send_req(V2Url ++ "/keys" ++ QueryStr, Header, put, Body, [], 5000) of
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
            case gen_server:call(?MODULE, {peer_down}) of
                undefined ->
                    {fail, peer_down};
                NewPeer ->
                    etcd_action(set, NewPeer ++ "/v2/keys", Opts)
            end;
        Reason ->
            {fail, Reason}
    end;
etcd_action(get, V2Url, Opts) ->
    OptStr = generate_read_str_from_opts(Opts),
    case ibrowse:send_req(V2Url ++ "/keys" ++ OptStr, [], get, [], [], 5000) of
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
            case gen_server:call(?MODULE, {peer_down}) of
                undefined ->
                    {fail, peer_down};
                NewPeer ->
                    etcd_action(get, NewPeer ++ "/v2/keys", Opts)
            end;
        Reason ->
            {fail, Reason}
    end;
etcd_action(delete, V2Url, Opts) ->
    {_, OptStr} = generate_modify_url_and_data_from_opts(
        Opts#etcd_modify_opts{recursive = true, refresh = undefined}) ,
    case ibrowse:send_req(V2Url ++ "/keys" ++ OptStr, [], delete, [], [], 5000) of
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
            case gen_server:call(?MODULE, {peer_down}) of
                undefined ->
                    {fail, peer_down};
                NewPeer ->
                    etcd_action(delete, NewPeer ++ "/v2/keys", Opts)
            end;
        Reason ->
            {fail, Reason}
    end.


generate_read_str_from_opts(Opts) ->
    case Opts of
        #etcd_read_opts{
            key = Key,
            wait = Wait,
            recursive = Recursive,
            sorted = Sorted,
            modified_index = ModifiedIndex} ->

            OptList0 = case is_boolean(Wait) of
                true -> ["wait=" ++ atom_to_list(Wait)];
                _ -> []
            end,
            OptList1 = case is_boolean(Recursive) of
                true -> OptList0 ++ ["recursive=" ++ atom_to_list(Recursive)];
                _ -> OptList0
            end,
            OptList2 = case is_boolean(Sorted) of
                true -> OptList1 ++ ["sorted=" ++ atom_to_list(Sorted)];
                _ -> OptList1
            end,
            OptList3 = case is_integer(ModifiedIndex) of
                true ->
                    OptList2 ++ ["waitIndex=" ++ integer_to_list(ModifiedIndex)];
                false ->
                    OptList2
            end,
            OptsStr = lists:foldl(fun gen_query_str/2, "", OptList3),

            Key ++ OptsStr;
        _ ->
            %% Actually , you should raise an exception here,
            %% but let's just ignore it so the gen server can just return a 404
            ""
    end.

gen_query_str(OptStr, CurStr) ->
    case {OptStr, CurStr} of
        {"", _} -> CurStr;
        {_, ""} -> "?" ++ OptStr;
        {_,_} -> CurStr ++ "&" ++ OptStr
    end.

generate_modify_url_and_data_from_opts(Opts) ->
    case Opts of
        #etcd_modify_opts{
            ttl = TTL,        
            key = Key,
            value = Value,      
            recursive = Recursive,      
            refresh = Refresh,
            prev_value = PrevValue,
            prev_index = PrevIndex,
            prev_exist = PrevExist,
            dir = Dir
            } ->
            DataStr = case {Value, TTL }of
                {undefined, undefined} -> "";
                {_, undefined} -> "value=" ++ Value;
                {undefined, _} -> "ttl=" ++ integer_to_list(TTL);
                {_, _} -> "value=" ++ Value ++ "&ttl=" ++ integer_to_list(TTL)
            end,

            QueryList0 = case is_boolean(Recursive) of
                true -> ["recursive=" ++ atom_to_list(Recursive)];
                false -> []
            end,
            QueryList1 = case is_boolean(Refresh) of
                true -> QueryList0 ++ ["refresh=" ++ atom_to_list(Refresh)];
                false -> QueryList0
            end,
            QueryList2 = case is_boolean(PrevExist) of
                true -> QueryList1 ++ ["prevExist=" ++ atom_to_list(PrevExist)];
                false -> QueryList1
            end,
            QueryList3 = case is_boolean(Dir) of
                true -> QueryList2 ++ ["dir=" ++ atom_to_list(Dir)];
                false -> QueryList2
            end,
            QueryList4 = case is_list(PrevValue) of
                true -> QueryList3 ++ ["prevValue=" ++ PrevValue];
                false -> QueryList3
            end,
            QueryList5 = case is_integer(PrevIndex) of
                true -> QueryList4 ++ ["prevIndex=" ++ integer_to_list(PrevIndex)];
                false -> QueryList4
            end,
            QueryStr = lists:foldl(fun gen_query_str/2, "", QueryList5),

            {DataStr, Key ++ QueryStr};
        _ -> {"", ""}
    end.

