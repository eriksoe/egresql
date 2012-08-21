-module(egresql_server_session).
-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("egres_wire_protocol.hrl").

start_link(SocketFun) ->
    error_logger:info_msg("Session: in start_link()\n", []),
    {ok,Pid} = gen_server:start_link(?MODULE, [SocketFun], []),
    {ok,Pid}.

%%====================================================================
%% State
%%====================================================================

-record(state, {
          socket :: port(),
          inbuffer = handshake :: handshake | normal | {byte(),integer(),binary()},
          instate = handshake :: handshake | normal,

          database_name, database_pid,
          username, salt
        }).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([SocketFun]) ->
    error_logger:info_msg("Session: in init()\n", []),
    Socket = SocketFun(),
    error_logger:info_msg("Session: got a connection; ~p\n", [Socket]),
    {ok, #state{socket = Socket,
                salt=crypto:rand_bytes(4)
               }}.

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
    error_logger:info_msg("session: got unexpected ~p (state=~p}\n", [_Info, State]),
    {noreply, State}.

terminate(Reason, _State) ->
    error_logger:error_msg("~s closed: ~p\n", [?MODULE, Reason]),
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
    ClientParams = binary:split(Rest1, <<0>>, [global]),
    %% TODO: Store username and database.
    error_logger:info_msg("session handshake: settings = ~p\n",
                          [ClientParams]),
    State2 = remember_client_params(ClientParams, State),

    %% Send handshake response:
    if {Major,Minor} /= {3,0} ->
            send_packet(State2, ?PG_MSGTYPE_ERROR,
                        <<"Unsupported protocol version">>),
            error({unsupported_protocol_version, {Major, Minor}});
       true ->
            %send_dummy_authrequest(State2),
            %% TODO: Use salt
            send_md5_authrequest(State2, State2#state.salt),
            %send_dummy_error(State2),
            State2#state{instate=normal}
    end;
handle_packet(?PG_MSGTYPE_PASSWORD, Packet, State) ->
    %% Parse:
    PwdHashLen = byte_size(Packet)-4,
    <<"md5", InPwdHash:PwdHashLen/binary, 0>> = Packet,

    %% Ask:
    AuthResult = egresql_server_auth:authenticate_by_md5(
                   State#state.database_name,
                   State#state.username,
                   State#state.salt,
                   InPwdHash),

    error_logger:info_msg("Auth response: ~p ; authresult: ~p\n",
                          [InPwdHash, AuthResult]),
    case AuthResult of
        {ok, DBPid} ->
            send_authrequest_ok(State),
            %% TODO: Make the initial control flow more statemachine-like.
            %% Send ready-signal:
            send_packet(State, ?PG_MSGTYPE_SAVE_PARAMETER,
                        <<"server_version",0,"1.2.3",0>>),
            send_packet(State, ?PG_MSGTYPE_READY_FOR_QUERY,
                        <<?PG_XACTSTATUS_IDLE>>),
            State#state{database_pid=DBPid};
        false ->
            send_auth_error(State),
            exit(normal)
    end;
handle_packet(?PG_MSGTYPE_QUERY, Packet, State) ->
    QueryLen = byte_size(Packet)-1,
    <<Query:QueryLen/binary, 0>> =  Packet,
    error_logger:info_msg("Got query: ~s\n", [Query]),
    QueryStr = binary_to_list(Query),
    try parse_commands(QueryStr) of
        {ok, ParsedQuery} ->
            error_logger:info_msg("Parsed query: ~p\n", [ParsedQuery]),
            ResText = list_to_binary(io_lib:format("~w", [ParsedQuery])),
            send_dummy_query_result(State, ResText);
        {parse_error, Line, ErrMsg} ->
            ResText = list_to_binary(io_lib:format("In line ~b: ~s", [Line, ErrMsg])),
            send_error(State, <<"Error">>, ResText, [])
    catch
        _:ParseError ->
            error_logger:warning_msg("Fail to parse query: ~p\n", [ParseError]),
            send_dummy_error(State)
    end,
    send_packet(State, ?PG_MSGTYPE_READY_FOR_QUERY, <<?PG_XACTSTATUS_IDLE>>),
    State;
handle_packet(?PG_MSGTYPE_GOODBYE, <<>>, _State) ->
    exit(normal);
handle_packet(MsgType, Packet, _State) ->
    error_logger:error_msg("Cannot handle msgtype ~p (data ~p)\n",
                           [[MsgType], Packet]),
    error({unhandled_msgtype, MsgType}).


%%%========== Packet types ===================================
send_error(State, Severity, Message, _Extra) ->
    %% TODO: Handle extras (details, hints, etc.)
    send_packet(State, ?PG_MSGTYPE_ERROR,
                <<"S", Severity/binary, 0,
                  "M", Message/binary, 0,
                  0>>).

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
                <<?PG_AUTHREQ_MD5:32, Salt/binary>>).

send_dummy_query_result(State, Text) ->
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
    TextLen = byte_size(Text),
    send_packet(State, $D,
                <<ColCount:16, % Tuple size
                  %% Value 1:
                  TextLen:32, % Value length
                  Text/binary>>),
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
    gen_tcp:send(Sck, Data),
    error_logger:info_msg("session do_send_packet: sent.\n", []),
    ok.

%%%========== State management ============================================

remember_client_params(RawClientParams, State) ->
    ClientParams = pair_up_client_params(RawClientParams, []),
    case {lists:keyfind(<<"database">>, 1, ClientParams),
          lists:keyfind(<<"user">>, 1, ClientParams)} of
        {false, _} -> error({no_database_param_supplied, ClientParams});
        {_, false} -> error({no_username_param_supplied, ClientParams});
        {{_,DatabaseName}, {_,UserName}} ->
            State#state{database_name=DatabaseName, username=UserName}
    end.

pair_up_client_params([<<>>, <<>>], Acc) -> Acc;
pair_up_client_params([Key, Value | Rest], Acc) ->
    pair_up_client_params(Rest, [{Key, Value} | Acc]).


%%%==================== Parsing ========================================
parse_commands(Str) ->
    case egresql_lexer:string(Str) of
        {ok, Tokens, _EndLine} ->
            case egresql_parser:parse(Tokens) of
                    {ok, Parsed} ->
                    {ok, Parsed};
                {error, {Line, _, Chars}} ->
                    {parse_error, Line, lists:flatten(io_lib:format("~s", [Chars]))}
            end;
        {error, {Line, _, {illegal, Chars}},_} ->
            {parse_error, Line, lists:flatten(io_lib:format("illegal token: ~s", [Chars]))}
    end.
