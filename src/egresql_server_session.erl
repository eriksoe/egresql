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
          inbuffer = handshake :: handshake | normal | {byte(),integer(),binary()},
          instate = handshake :: handshake | normal,

          username, salt
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
            {ok, #state{socket = Socket,
                        salt=crypto:rand_bytes(4)
                       }}
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
handle_info({tcp_closed, Sck}, State=#state{socket=Sck}) ->
    error_logger:info_msg("session closed.\n", []),
    {stop, normal, State};
handle_info({incoming_packet, MsgType, Packet}, State) when is_binary(Packet) ->
    error_logger:info_msg("session: got packet ~p\n", [Packet]),
    {noreply, handle_packet(MsgType, Packet, State)};
handle_info(_Info, State=#state{}) ->
    %% TODO: Handle {tcp,Port,Data}
    %% TODO: Handle {tcp_closed,Port}.
    error_logger:info_msg("session: got unexpected ~p (state=~p}\n", [_Info, State]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%% TODO: Can this handle if msglen or <<msgtype,msglen>> is on a packet boundary?
packetize(<<>>, State) -> State;
packetize(<<TotalLen:32/big, Rest1/binary>>, State=#state{inbuffer=handshake}) ->
    BodyLen = TotalLen-4,
    %% Go to the normal case, except that the message type is 'handshake':
    io:format("DB| packetize HSHK: ~p/~p\n", [Rest1, State#state{inbuffer={handshake,BodyLen,<<>>}}]),
    packetize(Rest1, State#state{inbuffer={handshake,BodyLen,<<>>}});
packetize(<<MsgType, TotalLen:32/big, Rest1/binary>>, State=#state{inbuffer=normal}) ->
    BodyLen = TotalLen-4,
    case Rest1 of
        <<Pkt:BodyLen/binary, Rest2/binary>> ->
            error_logger:info_msg("packetize case 1\n", []),
            %% Buffer empty, got complete packet:
            self() ! {incoming_packet, MsgType, Pkt},
            packetize(Rest2, State);
        PktPart ->
            error_logger:info_msg("packetize case 2 (bodylength=~p)\n", [BodyLen]),
            %% Buffer empty, got incomplete packet:
            State#state{inbuffer = {MsgType, BodyLen, PktPart}}
    end;
packetize(NewData, State=#state{inbuffer={MsgType,Len,BufData}}) ->
    Take = Len - byte_size(BufData),
    case NewData of
        <<Taken:Take/binary, Rest/binary>> ->
            %% Complete packet:
            error_logger:info_msg("packetize case 3\n", []),
            self() ! {incoming_packet, MsgType, <<BufData/binary, Taken/binary>>},
            packetize(Rest, State#state{inbuffer = normal});
        Taken ->
            %% Still not enough:
            error_logger:info_msg("packetize case 4\n", []),
            State#state{inbuffer = {MsgType, Len, <<BufData/binary, Taken/binary>>}}
    end.

handle_packet(handshake, Packet, State) ->
    <<Major:16, Minor:16, Rest1/binary>> = Packet,
    error_logger:info_msg("session handshake: client version is ~b:~b\n",
                          [Major, Minor]),
    ClientSettings = binary:split(Rest1, <<0>>, [global]),
    %% TODO: Store username and database.
    error_logger:info_msg("session handshake: settings = ~p\n",
                          [ClientSettings]),

    %% Send handshake response:
    if {Major,Minor} /= {3,0} ->
            send_packet(State, ?PG_MSGTYPE_ERROR,
                        <<"Unsupported protocol version">>),
            error({unsupported_protocol_version, {Major, Minor}});
       true ->
            %send_dummy_authrequest(State),
            %% TODO: Use salt
            send_md5_authrequest(State, 16#30313233),
            %send_dummy_error(State),
            State#state{instate=normal}
    end;
handle_packet(?PG_MSGTYPE_PASSWORD, Packet, State) ->
    %% TODO: Use stored username and salt; get expected password from some source.
    Expected = md5_auth(<<"erik">>,<<"password">>, <<"0123">>),
    PwdHashLen = byte_size(Packet)-4,
    <<"md5", InPwdHash:PwdHashLen/binary, 0>> =  Packet,
    ChecksOut = InPwdHash=:=Expected,
    error_logger:info_msg("Auth response: ~p ; expected ~p ; mathes: ~p\n",
                          [InPwdHash, Expected, ChecksOut]),
    if ChecksOut ->
            send_authrequest_ok(State),
            %% TODO: We don't change the state here... that's obviously wrong. This is only so because at the time being we focus on getting server and client to talk to each other.
            %% TODO: Make the initial control flow more statemachine-like.
            %% Send ready-signal:
            send_packet(State, ?PG_MSGTYPE_SAVE_PARAMETER,
                        <<"server_version",0,"1.2.3",0>>),
            send_packet(State, ?PG_MSGTYPE_READY_FOR_QUERY,
                        <<?PG_XACTSTATUS_IDLE>>),
            State;
       true ->
            send_auth_error(State),
            exit(normal)
    end;
handle_packet(?PG_MSGTYPE_QUERY, Packet, State) ->
    QueryLen = byte_size(Packet)-1,
    <<Query:QueryLen/binary, 0>> =  Packet,
    error_logger:info_msg("Got query: ~s\n", [Query]),
    send_dummy_query_result(State),
    send_packet(State, ?PG_MSGTYPE_READY_FOR_QUERY, <<?PG_XACTSTATUS_IDLE>>),
    State;
handle_packet(MsgType, Packet, State) ->
    error_logger:error_msg("Cannot handle msgtype ~p (data ~p)\n",
                           [[MsgType], Packet]),
    error({unhandled_msgtype, MsgType}).


%%%========== Packet types ===================================
send_dummy_error(State) ->
    send_packet(State, ?PG_MSGTYPE_ERROR,
                <<"SSeverity", 0, "MMessage", 0, "DDetail", 0, "HHint", 0, 0
>>).

send_auth_error(State) ->
    send_packet(State, ?PG_MSGTYPE_ERROR,
                <<"SError", 0, "MAuthentication failed.", 0, 0>>).

send_authrequest_ok(State) ->
    send_packet(State, ?PG_MSGTYPE_AUTHREQUEST,
                <<?PG_AUTHREQ_OK:32>>).

send_md5_authrequest(State, Salt) ->
    send_packet(State, ?PG_MSGTYPE_AUTHREQUEST,
                <<?PG_AUTHREQ_MD5:32, Salt:32>>).
send_dummy_query_result(State) ->
    ColCount = 1,
    ColName = <<"column_name">>,
    TableID = 16#4001, ColumnID = 1,
    ColType = ?PG_TYPE_TEXT, ColSize = -1,
    ColAttr = -1, ColFormat = 0,
    send_packet(State, $T, <<ColCount:16,
                             ColName/binary, 0,
                             TableID:32, ColumnID:16,
                             ColType:32, ColSize:16,
                             ColAttr:32, ColFormat:16>>),
    send_packet(State, $D,
                <<ColCount:16, % Tuple size
                %% Value 1:
                4:32, % Value length
                "foof">>),
    send_packet(State, $C, <<"SELECT", 0>>).

%%%========== Sending of packets ===================================
send_packet(State, Msg) when is_binary(Msg) ->
    Data = [<<(byte_size(Msg)+4):32>>, Msg],
    do_send_packet(State, Data).

send_packet(State, MsgType, Msg) when is_integer(MsgType), is_binary(Msg) ->
    Data = [MsgType, <<(byte_size(Msg)+4):32>>, Msg],
    do_send_packet(State, Data).

do_send_packet(#state{socket=Sck}, Data) ->
    error_logger:info_msg("session do_send_packet: ~p (~p bytes)\n", [Data, iolist_size(Data)]),
    gen_tcp:send(Sck, Data).

%%%========== Authentication ===================================
md5_auth(Username, Password, Salt) ->
    Digest1 = crypto:md5(<<Password/binary, Username/binary>>),
    Digest1Txt = binary_to_hex(Digest1),
    Digest2 = crypto:md5(<<Digest1Txt/binary, Salt/binary>>),
    binary_to_hex(Digest2).

binary_to_hex(Bin) ->
    << <<(hexchar(Hi)), (hexchar(Lo))>> || <<Hi:4, Lo:4>> <= Bin>>.

hexchar(X) when X < 10 -> $0 + X;
hexchar(X) -> $a + (X - 10).



