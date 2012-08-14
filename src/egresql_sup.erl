
-module(egresql_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(ServerPort) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, {ServerPort}).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init({ServerPort}) ->
    {ok, {{one_for_one, 5, 10},
          [
           %% {Id,StartFunc,Restart,Shutdown,Type,Modules}
           {egresql_server_listener,
            {egresql_server_listener, start_link, [ServerPort]},
            permanent, 5000, worker, [egresql_server_listener]}
          ]} }.

