-module(egresql_app).

-behaviour(application).

-define(DEFAULT_PORT, 7878).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ServerPort = case application:get_env(egresql, server_port) of
               undefined -> ?DEFAULT_PORT;
               P when is_integer(P) -> P
           end,
    egresql_sup:start_link(ServerPort).

stop(_State) ->
    ok.
