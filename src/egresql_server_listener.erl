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
    process_flag(trap_exit, true),
    case gen_tcp:listen(Port, [binary, {reuseaddr, true}]) of
        {ok, LSocket} ->
            proc_lib:init_ack({ok,self()}),
            error_logger:info_msg("Listening on port ~p: ~p\n", [Port,LSocket]),
            accept_then_loop(LSocket);
        {error, Reason} ->
            error_logger:error_msg("Listener: listen() on port ~p failed: ~p\n", [Port,Reason]),
            error({listen_failed, Reason})
    end.

accept_then_loop(LSocket) ->
    Me = self(),
    Ref = make_ref(),
    {ok, AcceptorPid} = egresql_server_session:start_link(fun() -> {ok, Socket} = gen_tcp:accept(LSocket),
                                               Me ! {accepted, Ref},
                                               Socket
                                      end),
    loop(LSocket).

loop(LSocket) ->
    receive
        {accepted, Ref} ->
            %% Got incoming connection - acceptor is taken now.
            accept_then_loop(LSocket);
        {'EXIT', AcceptorPid, _Reason} ->
            %% Acceptor disappeared for some reason.
            loop(LSocket);
        {'EXIT', _OtherPid, normal} ->
            loop(LSocket);
        {'EXIT', _OtherPid, OtherReason} ->
            error_logger:warning_msg("Server listener: Shutting down - received exit signal ~p\n", [OtherReason]),
            exit(shutdown)
    end.

%%     case gen_tcp:accept(LSocket) of
%%         {ok, Socket} ->
%%             error_logger:info_msg("Got incoming connection: ~p\n", [Socket]),
%%             egresql_server_session:start_link(Socket),
%%             accept_loop(LSocket);
%%         {error, Reason} ->
%%             error_logger:error_msg("Listener: accept() failed: ~p\n", [Reason]),
%%             error({accept_failed, Reason})
%%     end.
