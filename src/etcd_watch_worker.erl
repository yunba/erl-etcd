%%%-------------------------------------------------------------------
%%% @author zhongwen
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(etcd_watch_worker).

-behaviour(gen_server).

-export([start_watch/4]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([test/0]).
-include("etcd.hrl").

-record(state, {event_flag, opts, pid, peer = [], call_ref, hackney_ref, status, time = 0}).
-define(CLEAR_INDEX(O), Opts#etcd_read_opts{modified_index = undefined}).

test() ->
    application:ensure_all_started(etcd),
    logger:set_primary_config(level, info),
    Key = "/zhonwen",
    {ok, Pid} = etcd:watch(Key, self(), etcd_event),
    %% Opts = #etcd_read_opts{key = Key, modified_index = 590},
    %% {ok, Pid} = etcd:watch(Opts, self()),
    logger:info("watch pid:~p", [Pid]),
    timer:sleep(100),
    etcd:set(Key, "good"),
    ok.

start_watch(Url, Opts, Pid, EventFlag) ->
    gen_server:start_link(?MODULE, [Url, Opts, Pid, EventFlag], []).

init([Url, Opts, Pid, EventFlag]) ->
    Ref = erlang:monitor(process, Pid),
    erlang:process_flag(trap_exit, true),
    {ok, #state{opts = Opts, pid = Pid, peer = Url,
        call_ref = Ref, event_flag = EventFlag}, 0}.

handle_call(_Request, _From, State = #state{}) ->
    {reply, ok, State}.

handle_cast(_Request, State = #state{}) ->
    {noreply, State}.

handle_info(timeout, State) ->
    NewState = do_watch(State),
    {noreply, NewState};
handle_info(re_watch, State) ->
    NewState = do_watch(State),
    {noreply, NewState};

handle_info({hackney_response, Ref, {status, Status, _Reason}},
    State = #state{hackney_ref = Ref}) ->
    hackney:stream_next(Ref),
    logger:info("watch status: ~p", [Status]),
    {noreply, State#state{status = Status}};

handle_info({hackney_response, Ref, {headers, Headers}},
    State = #state{hackney_ref = Ref, opts = Opts, status = Status}) ->
    logger:info("watch header: ~p ~p", [Opts, Headers]),
    case Status of
        200 ->
            hackney:stream_next(Ref),
            {noreply, State};
        400 ->
            {ok, Ref} = hackney:stop_async(Ref),
            {error, req_not_async} = hackney_manager:async_response_pid(Ref),
            {ok, Body} = hackney:body(Ref),
            logger:info("watch 400 ~p: ~p ~p ", [Status, Body, Opts]),
            try get_modified_index_from_400_response_body(Body) of
                {ok, NewModifiedIndex} ->
                    NewIndex = NewModifiedIndex + 1,
                    rewatch_after(0),
                    {noreply, State#state{time = 0,
                    opts = Opts#etcd_read_opts{modified_index = NewIndex}}};
                _ ->
                    rewatch_after(100),
                    {noreply, State#state{time = 100, peer = "", opts = ?CLEAR_INDEX(Opts)}}
            catch
                E:R ->
                    rewatch_after(100),
                    logger:error("watch 400 error ~p", [{E, R, Body}]),
                    {noreply, State#state{time = 100, peer = "", opts = ?CLEAR_INDEX(Opts)}}
            end;
        _ ->
            {ok, Ref} = hackney:stop_async(Ref),
            {error, req_not_async} = hackney_manager:async_response_pid(Ref),
            {ok, Body} = hackney:body(Ref),
            logger:info("watch error ~p: ~p ~p ", [Status, Body, Opts]),
            rewatch_after(100),
            {noreply, State#state{time = 100, peer = "", opts = ?CLEAR_INDEX(Opts)}}
    end;

handle_info({hackney_response, Ref, Body}, State
    = #state{hackney_ref = Ref, opts = Opts, pid = Pid, event_flag = EventFlag})
    when is_binary(Body) ->
    hackney:stream_next(Ref),
    try get_modified_index_from_response_body(Body) of
        {ok, NewModifiedIndex, Json} ->
            NewIndex = NewModifiedIndex + 1,
            erlang:send(Pid, {EventFlag, Json}),
            {noreply, State#state{opts = Opts#etcd_read_opts{modified_index = NewIndex}, time = 0}};
        _ ->
            {noreply, State#state{peer = "", opts = ?CLEAR_INDEX(Opts), time = 100}}
    catch
        E:R ->
            logger:error("error:~p", [{E, R, Body}]),
            {noreply, State#state{peer = "", opts = ?CLEAR_INDEX(Opts), time = 200}}
    end;

handle_info({hackney_response, Ref, done},
    State = #state{hackney_ref = Ref, time = Time}) ->
    logger:info("watch DONE ~p ~p", [Ref, Time]),
    rewatch_after(Time),
    {noreply, State};

handle_info({hackney_response, Ref, Error},
    State = #state{hackney_ref = Ref, time = Time}) ->
    logger:error("watch Error ~p ~p", [Ref, Error, Time]),
    rewatch_after(400),
    {noreply, State#{time = 0}};

handle_info(stop, State) ->
    {stop, normal, State};

handle_info({'DOWN', Ref, _, _, _}, State = #state{call_ref = Ref}) ->
    {stop, normal, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, #state{hackney_ref = Ref, opts = Opts}) ->
    erlang:is_reference(Ref) andalso hackney:cancel_request(Ref),
    logger:info("~p ~p watch stop by ~p", [Ref, Opts, Reason]),
    ok.

code_change(_OldVsn, State = #state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_watch(State) ->
    #state{peer = Peer, opts = Opts} = State,
    case get_healthy_url(Peer) of
        "" -> rewatch_after(1000);
        V2Url ->
            OptStr = etcd_worker:generate_read_str_from_opts(Opts#etcd_read_opts{wait = true}),
            try hackney:get(V2Url ++ "/v2/keys" ++ OptStr, [], <<>>,
                [{async, once}, {recv_timeout, infinity}, {pool, etcd}]) of
                {ok, HackneyRef} ->
                    State#state{hackney_ref = HackneyRef};
                {error, econnrefused} ->
                    etcd_worker ! peer_down,
                    rewatch_after(100),
                    State#state{time = 100, peer = "", opts = ?CLEAR_INDEX(Opts)};
                _ ->
                    rewatch_after(200),
                    State#state{time = 200, peer = "", opts = ?CLEAR_INDEX(Opts)}
            catch
                _:_ ->
                    rewatch_after(200),
                    State#state{time = 200, peer = "", opts = ?CLEAR_INDEX(Opts)}
            end
    end.

get_healthy_url("") ->
    case etcd:get_current_peer() of
        {ok, NewPeer} -> NewPeer;
        {error, _} -> ""
    end;
get_healthy_url(Peer) -> Peer.

rewatch_after(0) -> erlang:send(self(), re_watch);
rewatch_after(Ms) -> erlang:send_after(Ms, self(), re_watch).

get_modified_index_from_response_body(Body) ->
    case jiffy:decode(Body, [return_maps]) of
        #{} = Maps ->
            Value =
                case maps:find(<<"node">>, Maps) of
                    {ok, NodeValue} ->
                        {ok, Index} = maps:find(<<"modifiedIndex">>, NodeValue),
                        Index;
                    error ->
                        %% let me know if there is any possible that a watch will a list of nodes
                        lists:foldl(fun(NewValue, MaxModifyIndex) ->
                            {ok, CurModifiedIndex} = maps:find(<<"modifiedIndex">>, NewValue),
                            case (CurModifiedIndex > MaxModifyIndex) of
                                true -> CurModifiedIndex;
                                false -> MaxModifyIndex
                            end
                                    end, 0, Maps)
                end,
            {ok, Value, Maps};
        Err ->
            {fail, wrong_json_body, Err}
    end.

get_modified_index_from_400_response_body(Body) ->
    case jiffy:decode(Body, [return_maps]) of
        #{} = Maps ->
            case maps:find(<<"errorCode">>, Maps) of
                {ok, 401} ->
                    maps:find(<<"index">>, Maps);
                _ ->
                    {fail, undefined}
            end;
        Err -> {fail, wrong_response_code, Err}
    end.