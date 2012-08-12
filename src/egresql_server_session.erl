-module(egresql_server_session).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("egres_wire_protocol.hrl").

start_link(Socket) ->
    error_logger:info_msg("Session: in start_link()\n", []),
    {ok,Pid} = gen_server:start_link(?MODULE, [], []),
    error_logger:info_msg("Session: after spawn\n", []),
    ok = gen_tcp:controlling_process(Socket, Pid),
    Pid ! {'$init', Socket},
    {ok,Pid}.

%%====================================================================
%% State
%%====================================================================

-record(state, {
          socket :: port(),
          inbuffer = undefined :: undefined | {integer(),binary()},
          instate = handshake :: handshake | normal
        }).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    error_logger:info_msg("Session: in init()\n", []),
    proc_lib:init_ack({ok, self()}),
    receive
        {'$init', Socket} ->
            error_logger:info_msg("Session started: ~p\n", [self()]),
            {ok, #state{socket = Socket}}
    after 5000 ->
            error_logger:error_msg("Timed out waiting for socket to be passed to process ~p\n", [self()]),
            error(timeout_waiting_for_socket_to_be_passed)
    end.

handle_call(_Request, _From, State) ->
    error_logger:info_msg("session: got call ~p\n", [_Request]),
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    error_logger:info_msg("session: got cast ~p\n", [_Msg]),
    {noreply, State}.

handle_info({tcp, Sck, Data}, State=#state{socket=Sck}) ->
    error_logger:info_msg("session: got raw: ~p\n", [Data]),
    {noreply, packetize(Data, State)};
handle_info({tcp_closed, Data}, State=#state{socket=Sck}) ->
    error_logger:info_msg("session closed.\n", []),
    {stop, normal, State};
handle_info({incoming_packet, Packet}, State) when is_binary(Packet) ->
    error_logger:info_msg("session: got packet ~p\n", [Packet]),
    {noreply, handle_packet(Packet, State)};
handle_info(_Info, State) ->
    %% TODO: Handle {tcp,Port,Data}
    %% TODO: Handle {tcp_closed,Port}.
    error_logger:info_msg("session: got ~p\n", [_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

packetize(<<>>, State) -> State;
packetize(<<TotalLen:32/big, Rest1/binary>>, State=#state{inbuffer=undefined}) ->
    BodyLen = TotalLen-4,
    case Rest1 of
        <<Pkt:BodyLen/binary, Rest2/binary>> ->
            error_logger:info_msg("packetize case 1\n", []),
            %% Buffer ampty, got complete packet:
            self() ! {incoming_packet, Pkt},
            packetize(Rest2, State);
        PktPart ->
            error_logger:info_msg("packetize case 2 (bodylength=~p)\n", [BodyLen]),
            %% Buffer ampty, got incomplete packet:
            State#state{inbuffer = {BodyLen, PktPart}}
    end;
packetize(NewData, State=#state{inbuffer={Len,BufData}}) ->
    Take = Len - byte_size(BufData),
    case NewData of
        <<Taken:Take/binary, Rest/binary>> ->
            %% Complete packet:
            error_logger:info_msg("packetize case 3\n", []),
            self() ! {incoming_packet, <<BufData/binary, Taken/binary>>},
            packetize(Rest, State#state{inbuffer = undefined});
        Taken ->
            %% Still not enough:
            error_logger:info_msg("packetize case 4\n", []),
            State#state{inbuffer = {Len, <<BufData/binary, Taken/binary>>}}
    end.

handle_packet(Packet, State=#state{instate=handshake}) ->
    <<Major:16, Minor:16, Rest1/binary>> = Packet,
    error_logger:info_msg("session handshake: client version is ~b:~b\n",
                          [Major, Minor]),
    ClientSettings = binary:split(Rest1, <<0>>, [global]),
    error_logger:info_msg("session handshake: settings = ~p\n",
                          [ClientSettings]),

    %% Send handshake response:
    if {Major,Minor} /= {3,0} ->
            send_packet(State, ?PG_MSGTYPE_ERROR,
                        <<"Unsupported protocol version">>),
            error({unsupported_protocol_version, {Major, Minor}});
       true ->
            %% Dummy failure:
            send_packet(State, ?PG_MSGTYPE_ERROR,
                        <<"SSeverity", 0, "MMessage", 0, "DDetail", 0, "HHint", 0, 0
>>),
            State#state{instate=normal}
    end.

%%%========== Sending of packets ===================================
send_packet(State, Msg) when is_binary(Msg) ->
    Data = [<<(byte_size(Msg)+4):32>>, Msg],
    do_send_packet(State, Data).

send_packet(State, MsgType, Msg) when is_integer(MsgType), is_binary(Msg) ->
    Data = [MsgType, <<(byte_size(Msg)+4):32>>, Msg],
    do_send_packet(State, Data).

do_send_packet(State=#state{socket=Sck}, Data) ->
    error_logger:info_msg("session do_send_packet: ~p (~p bytes)\n", [Data, iolist_size(Data)]),
    gen_tcp:send(Sck, Data).


