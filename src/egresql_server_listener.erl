-module(egresql_server_listener).

%%====================================================================
%% API
%%====================================================================
-export([start_link/1]).

%% Exported for spawn:
-export([init/1]).

start_link(Port) ->
    proc_lib:start_link(?MODULE, init, [Port]).

%%====================================================================
%% Internals
%%====================================================================

init(Port) ->
    case gen_tcp:listen(Port, [binary, {reuseaddr, true}]) of
        {ok, LSocket} ->
            proc_lib:init_ack({ok,self()}),
            error_logger:info_msg("Listening on port ~p: ~p\n", [Port,LSocket]),
            accept_loop(LSocket);
        {error, Reason} ->
            error_logger:error_msg("Listener: listen() on port ~p failed: ~p\n", [Port,Reason]),
            error({listen_failed, Reason})
    end.

accept_loop(LSocket) ->
    case gen_tcp:accept(LSocket) of
        {ok, Socket} ->
            error_logger:info_msg("Got incoming connection: ~p\n", [Socket]),
            egresql_server_session:start_link(Socket),
            accept_loop(LSocket);
        {error, Reason} ->
            error_logger:error_msg("Listener: accept() failed: ~p\n", [Reason]),
            error({accept_failed, Reason})
    end.
