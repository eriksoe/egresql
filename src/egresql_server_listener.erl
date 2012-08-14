-module(egresql_server_listener).
-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%====================================================================
%% API
%%====================================================================

start_link(Port) ->
    gen_server:start_link({local,?MODULE}, ?MODULE, {Port}, []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-record(state,
        {lsocket :: port(),
         myref :: reference(),
         acceptors :: [pid()]
        }).

init({Port}) ->
    process_flag(trap_exit, true),
    case gen_tcp:listen(Port, [binary, {reuseaddr, true}]) of
        {ok, LSocket} ->
            proc_lib:init_ack({ok,self()}),
            error_logger:info_msg("Listening on port ~p: ~p\n", [Port,LSocket]),
            State1 = #state{lsocket = LSocket,
                            myref = make_ref(),
                            acceptors = []},
            State2 = start_acceptor(State1),
            {ok, State2};
        {error, Reason} ->
            error_logger:error_msg("Listener: listen() on port ~p failed: ~p\n", [Port,Reason]),
            {error, {listen_failed, Reason}}
    end.

handle_call(Request, From, State) ->
    error_logger:error_msg("~s: Got unexpected call from ~p: ~p\n", [?MODULE, From, Request]),
    {noreply, State}.

handle_cast(Msg, State) ->
    error_logger:error_msg("~s: Got unexpected cast: ~p\n", [?MODULE, Msg]),
    {noreply, State}.

handle_info({accepted, Ref}, State=#state{myref=MyRef}) when Ref=:=MyRef ->
    %% Got incoming connection - acceptor is taken now.
    error_logger:info_msg("Acceptor got one\n", []),
    State2 = start_acceptor(State),
    {noreply, State2};
handle_info({'DOWN', _MRef, process, Pid, Reason}, State=#state{acceptors=Acceptors}) ->
    error_logger:info_msg("~s: Got DOWN message.\n", [?MODULE]),
    case lists:member(Pid, Acceptors) of
        true ->
            %% Acceptor disappeared for some reason.
            error_logger:info_msg("Acceptor went away\n", []),
            %% Clean up:
            State1 = State#state{acceptors=lists:delete(Pid, Acceptors)},
            unlink(Pid),
            receive {'EXIT', Pid, _} -> ok after 0 -> ok end,
            %% Replace the acceptor:
            State2 = start_acceptor(State1),
            {noreply, State2};
        false ->
            error_logger:info_msg("~s: Got spurious DOWN from ~p\n", [?MODULE, Pid]),
            {noreply, State}
    end;
handle_info({'EXIT', Pid, Reason}, State=#state{acceptors=Acceptors}) ->
    error_logger:warning_msg("~s: Got unexpected exit from ~p: ~p\n",
                             [?MODULE, Pid, Reason]),
    IsAcceptor = lists:member(Pid, Acceptors),
    if IsAcceptor;
       Reason==normal ->
            {noreply, State};
       true ->
            %% Regard as a stop signal
            error_logger:warning_msg("Server listener: Shutting down - received exit signal from ~p: ~p\n", [Pid,Reason]),
            {stop, shutdown, State}
    end;
handle_info(Msg, State) ->
    error_logger:warning_msg("~s: Got unexpected mesage: ~p\n",
                             [?MODULE, Msg]),
    {noreply, State}.

terminate(Reason, _State) ->
    error_logger:error_msg("~s closed: ~p\n", [?MODULE, Reason]),
    error_logger:error_msg("~s trace: ~p\n", [?MODULE, erlang:get_stacktrace()]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

start_acceptor(State=#state{lsocket=LSocket,
                            myref=Ref,
                           acceptors=Acceptors}) ->
    process_flag(trap_exit, true),
    Me = self(),
    SocketFun = fun() -> {ok, Socket} = gen_tcp:accept(LSocket),
                         Me ! {accepted, Ref},
                         Socket
                end,
    {ok, AcceptorPid} = egresql_server_session:start_link(SocketFun),
    _Mref = erlang:monitor(process, AcceptorPid),
    State#state{acceptors = [AcceptorPid | Acceptors]}.
