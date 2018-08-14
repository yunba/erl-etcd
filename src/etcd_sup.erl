-module(etcd_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, EtcdPeers}= application:get_env(etcd, addr), 
    {ok, {{one_for_one, 10, 10}, [
        ?CHILD(etcd_worker, worker, [EtcdPeers]),
        ?CHILD(etcd_watch_sup, supervisor, [])
    ]}}.

