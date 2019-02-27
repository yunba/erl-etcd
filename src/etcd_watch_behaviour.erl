-module(etcd_watch_behaviour).

-include("etcd.hrl").

-export([do_watch/3, start_watch/2]).

start_watch(Opts, Callback) ->
    Pid = proc_lib:spawn_link(?MODULE, do_watch, [undefined, Opts, Callback]),
    {ok, Pid}.

do_watch(Url, Opts, Callback) ->
    %% there is some chance that a peer is down so you will have to retrieve the url from etcd server
    V2Url =
        case Url of
            undefined ->
                Peer = etcd:get_current_peer(),
                Peer ++ "/v2";
            _ -> Url
        end,
    
    OptStr = etcd_worker:generate_read_str_from_opts(Opts#etcd_read_opts{wait = true}),
    
    try ibrowse:send_req(V2Url ++ "/keys" ++ OptStr, [], get, [], [], 60000) of
        {ok, ReturnCode, _Headers, Body} ->
            case ReturnCode of
                "200" ->
                    NewOpts =
                        try get_modified_index_from_response_body(Body) of
                            {ok, NewModifiedIndex} ->
                                NewIndex = NewModifiedIndex + 1,
                                Opts#etcd_read_opts{modified_index = NewIndex};
                            _ -> Opts
                        catch
                            _:_ -> Opts
                        end,
                    CallbackRet = Callback(Body),
                    case CallbackRet of
%% only stop will make the watching behaviour stop
                        ok -> do_watch(V2Url, NewOpts, Callback);
                        stop ->
                            ok;
                        _ -> do_watch(V2Url, NewOpts, Callback)
                    end;
                "400" ->
                    NewOpts =
                        try get_modified_index_from_400_response_body(Body) of
                            {ok, NewModifiedIndex} ->
                                NewIndex = NewModifiedIndex + 1,
                                Opts#etcd_read_opts{modified_index = NewIndex};
                            _ -> Opts
                        catch
                            _:_ -> Opts
                        end,
                    do_watch(V2Url, NewOpts, Callback);
                _ ->
                    do_watch(V2Url, Opts, Callback)
            end;
        {error, {conn_failed, {error, econnrefused}}} ->
            etcd_worker ! peer_down,
            do_watch("", Opts, Callback);
        _ ->
            do_watch(V2Url, Opts, Callback)
    catch
        _:_ ->
            do_watch(V2Url, Opts, Callback)
    end.

%% get_modified_index_from_400_response_body and get_modified_index_from_response_body
%% are used to get modified index from wait response
get_modified_index_from_400_response_body(Body) ->
    case jiffy:decode(Body) of
        {Props} ->
            ErrorCode = proplists:get_value(<<"errorCode">>, Props),
            case (ErrorCode == 401) of
                true ->
                    NewestIndex = proplists:get_value(<<"index">>, Props),
                    {ok, NewestIndex};
                false ->
                    {fail, undefined}
            end;
        _ -> {fail, wrong_response_code}
    end.

get_modified_index_from_response_body(Body) ->
    case jiffy:decode(Body) of
        {Props} ->
            %% no error, return value
            {NodeValue} = proplists:get_value(<<"node">>, Props),
            Nodes = proplists:get_value(<<"nodes">>, NodeValue),
            Value = case Nodes of
                        undefined ->
                            proplists:get_value(<<"modifiedIndex">>, NodeValue);
                        _ ->
                            %% let me know if there is any possible that a watch will a list of nodes
                            lists:foldl(fun(NewValue, MaxModifyIndex) ->
                                CurModifiedIndex = proplists:get_value(<<"modifiedIndex">>, NewValue),
                                case (CurModifiedIndex > MaxModifyIndex) of
                                    true -> CurModifiedIndex;
                                    false -> MaxModifyIndex
                                end
                                        end, 0, Nodes)
                    end,
            {ok, Value};
        _ ->
            {fail, wrong_json_body}
    end.
