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

-record(state, {peer = [], etcd = []}).

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
        [] -> {stop, no_etcd_peer_alive};
        _ -> {ok, #state{peer = Peer, etcd = EtcdPeers}}
    end.

handle_call({peer_down}, _From, State) ->
    #state{etcd = Etcd} = State,
    NewPeer = get_health_peer(Etcd),
    NewPeer =:= [] andalso erlang:send_after(2000, self(), peer_down),
    {reply, random_peers(NewPeer), State#state{peer = NewPeer}};

handle_call({peer}, _From, State = #state{peer = Peer}) ->
    {reply, random_peers(Peer), State};

handle_call({watch, Opts, Callback}, _From, State = #state{peer = Peer}) ->
    {ok, Pid} = etcd_watch_sup:add_child(Opts, random_peers(Peer), Callback),
    {reply, {ok, Pid}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(peer_down, State) ->
    #state{etcd = Etcd} = State,
    NewPeer = get_health_peer(Etcd),
    {noreply, State#state{peer = NewPeer}};

handle_info(_Info, State) ->
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
            case check_peer_alive(Url) of
                true -> [Url | ActivePeer];
                false -> ActivePeer
            end
        end, [], EtcdPeers).

check_peer_alive(Url) ->
    try hackney:request(get, Url ++ "/version", [], <<>>, [{recv_timeout, 5000}, {pool, etcd}, with_body]) of
        {ok, 200, _Headers, _Body} -> true;
        _ -> false
    catch
        _Exception:_Error -> false
    end.

random_peers([]) -> {error, no_etcd_peer_alive};
random_peers(List) -> {ok, lists:nth(rand:uniform(erlang:length(List)), List)}.

etcd_action(create, V2Url, Opts) ->
    {Body, QueryStr} = generate_modify_url_and_data_from_opts(Opts),
    try hackney:request(post, V2Url ++ "/keys" ++ QueryStr, [], Body, [{recv_timeout, 5000}, {pool, etcd}, with_body]) of
        {ok, 200, _Headers, RetBody} -> {ok, RetBody};
            {ok, 201, _Headers, RetBody} -> {ok, RetBody};
        {ok, ReturnCode, _Headers, RetBody} -> {fail, {wrong_response_code, ReturnCode, RetBody}};
        {error, econnrefused} ->
            case gen_server:call(?MODULE, {peer_down}) of
                {error, _} ->
                    {fail, peer_down};
                {ok, NewPeer} ->
                    etcd_action(create, NewPeer ++ "/v2", Opts)
            end;
        Reason -> {fail, Reason}
    catch
        Exception:Error -> {fail, {Exception, Error}}
    end;
etcd_action(set, V2Url, Opts) ->
    {Body, QueryStr} = generate_modify_url_and_data_from_opts(Opts),
    try hackney:request(put, V2Url ++ "/keys" ++ QueryStr, [], Body, [{recv_timeout, 5000}, {pool, etcd}, with_body]) of
        {ok, 200, _Headers, RetBody} -> {ok, RetBody};
        {ok, 201, _Headers, RetBody} -> {ok, RetBody};
        {ok, ReturnCode, _Headers, RetBody} -> {fail, {wrong_response_code, {ReturnCode, RetBody}}};
        {error, econnrefused} ->
            case gen_server:call(?MODULE, {peer_down}) of
                {error, _} ->
                    {fail, peer_down};
                {ok, NewPeer} ->
                    etcd_action(set, NewPeer ++ "/v2", Opts)
            end;
        Reason -> {fail, Reason}
    catch
        Exception:Error -> {fail, {Exception, Error}}
    end;
etcd_action(get, V2Url, Opts) ->
    OptStr = generate_read_str_from_opts(Opts),
    try hackney:request(get, V2Url ++ "/keys" ++ OptStr, [], [], [{recv_timeout, 5000}, {pool, etcd}, with_body]) of
        {ok, 200, _Headers, Body} -> {ok, Body};
        {ok, 404, _Headers, _Body} -> {fail, not_found};
        {ok, ReturnCode, _Headers, Body} -> {fail, {wrong_response_code, {ReturnCode, Body}}};
        {error, econnrefused} ->
            case gen_server:call(?MODULE, {peer_down}) of
                {error, _} ->
                    {fail, peer_down};
                {ok, NewPeer} ->
                    etcd_action(get, NewPeer ++ "/v2", Opts)
            end;
        Reason -> {fail, Reason}
    catch
        Exception:Error -> {fail, {Exception, Error}}
    end;
etcd_action(delete, V2Url, Opts) ->
    {_, OptStr} = generate_modify_url_and_data_from_opts(
        Opts#etcd_modify_opts{recursive = true, refresh = undefined}),
    try hackney:request(delete, V2Url ++ "/keys" ++ OptStr, [], [], [{recv_timeout, 5000}, {pool, etcd}, with_body]) of
        {ok, 200, _Headers, Body} -> {ok, Body};
        {ok, 404, _Headers, _Body} -> {fail, not_found};
        {ok, ReturnCode, _Headers, Body} -> {fail, {wrong_response_code, {ReturnCode, Body}}};
        {error, econnrefused} ->
            case gen_server:call(?MODULE, {peer_down}) of
                {error, _} ->
                    {fail, peer_down};
                {ok, NewPeer} ->
                    etcd_action(delete, NewPeer ++ "/v2", Opts)
            end;
        Reason -> {fail, Reason}
    catch
        Exception:Error -> {fail, {Exception, Error}}
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
            
            etcd_util:url_encode(Key) ++ OptsStr;
        _ ->
            %% Actually , you should raise an exception here,
            %% but let's just ignore it so the gen server can just return a 404
            ""
    end.

gen_query_str(OptStr, CurStr) ->
    case {OptStr, CurStr} of
        {"", _} -> CurStr;
        {_, ""} -> "?" ++ OptStr;
        {_, _} -> CurStr ++ "&" ++ OptStr
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
            DataStr = case {Value, TTL} of
                          {undefined, undefined} -> "";
                          {_, undefined} -> {form, [{"value", Value}]};
                          {undefined, _} -> {form, [{"ttl", TTL}]};
                          {_, _} -> {form, [{"value", Value}, {"ttl", TTL}]}
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
                             true -> QueryList3 ++ ["prevValue=" ++ etcd_util:url_encode(PrevValue)];
                             false -> QueryList3
                         end,
            QueryList5 = case is_integer(PrevIndex) of
                             true -> QueryList4 ++ ["prevIndex=" ++ integer_to_list(PrevIndex)];
                             false -> QueryList4
                         end,
            QueryStr = lists:foldl(fun gen_query_str/2, "", QueryList5),
            
            {DataStr, etcd_util:url_encode(Key) ++ QueryStr};
        _ -> {"", ""}
    end.