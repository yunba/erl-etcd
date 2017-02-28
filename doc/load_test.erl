-module(load_test).

-compile([export_all]).

start() ->
    group_spawn_test(50).

group_spawn_test(0) ->
    ok;
group_spawn_test(Counter) ->
    one_test(),
    group_spawn_test(Counter - 1).

one_test() ->
    {StartMegaSec, StartSec, StartMicroSec} = os:timestamp(),
    io:format("Start Time: ~p ~p ~p ~n", [StartMegaSec, StartSec, StartMicroSec]),
    load_test(1000),
    {EndMegaSec, EndSec, EndMicroSec} = os:timestamp(),
    io:format("End Time: ~p ~p ~p ~n", [EndMegaSec, EndSec, EndMicroSec]),
    io:format("Time spend: ~p ~n", [(EndSec - StartSec) * 1000000 + (EndMicroSec - StartMicroSec)]).

-define(LOAD_PREFIX, "/load_test/").
load_test(0) ->
    ok;
load_test(Counter) ->
    etcd:set(?LOAD_PREFIX ++ integer_to_list(Counter), integer_to_list(Counter)),
    etcd:get(?LOAD_PREFIX ++ integer_to_list(Counter)),
    etcd:delete(?LOAD_PREFIX ++ integer_to_list(Counter)),
    load_test(Counter - 1).

stop() ->
    ok.
