-module(etcd_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1, add_child/1, stop_child/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

add_child(ChildSpec) ->
    supervisor:start_child(?MODULE, ChildSpec).

stop_child(Pid) ->
    supervisor:terminate_child(?MODULE, Pid).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, EtcdPeers}= application:get_env(etcd, addr), 
    {ok, {{one_for_one, 10, 10}, [
        ?CHILD(etcd_worker, worker, [EtcdPeers])
    ]}}.

