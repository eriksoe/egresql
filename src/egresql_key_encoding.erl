-module(egresql_key_encoding).
-export([encode/2, decode/2]).

encode(integer, V) -> encode_int(V);
encode(string,  V) -> encode_string(V).

decode(integer, Bin) -> decode_int(Bin);
decode(string,  Bin) -> decode_string(Bin).

%%%==================== Integers: ========================================

encode_int(null) -> <<0>>;
%% Negatives:
encode_int(N) when N < 0, N >-16#100       -> <<16#7F, N:8>>;
encode_int(N) when N < 0, N >-16#10000     -> <<16#7E, N:16>>;
encode_int(N) when N < 0, N >-16#1000000   -> <<16#7D, N:24>>;
encode_int(N) when N < 0, N >-16#100000000 -> <<16#7C, N:32>>;
%% Zero:
encode_int(0) -> <<16#80>>;
%% Positives:
encode_int(N) when 0 < N, N < 16#100       -> <<16#81, N:8>>;
encode_int(N) when 0 < N, N < 16#10000     -> <<16#82, N:16>>;
encode_int(N) when 0 < N, N < 16#1000000   -> <<16#83, N:24>>;
encode_int(N) when 0 < N, N < 16#100000000 -> <<16#84, N:32>>.


decode_int(<<0,                    Rest/binary>>) -> {null, Rest};
decode_int(<<16#7C, N:32/unsigned, Rest/binary>>) -> {N - (1 bsl 32), Rest};
decode_int(<<16#7D, N:24/unsigned, Rest/binary>>) -> {N - (1 bsl 24), Rest};
decode_int(<<16#7E, N:16/unsigned, Rest/binary>>) -> {N - (1 bsl 16), Rest};
decode_int(<<16#7F, N:8 /unsigned, Rest/binary>>) -> {N - (1 bsl  8), Rest};
decode_int(<<16#80,                Rest/binary>>) -> {0, Rest};
decode_int(<<16#81, N:8 /unsigned, Rest/binary>>) -> {N, Rest};
decode_int(<<16#82, N:16/unsigned, Rest/binary>>) -> {N, Rest};
decode_int(<<16#83, N:24/unsigned, Rest/binary>>) -> {N, Rest};
decode_int(<<16#84, N:32/unsigned, Rest/binary>>) -> {N, Rest}.

%%%==================== Strings: ========================================
%%% The encoding needs to satisfy all of:
%%% - encoding is reversible;
%%% - null is encodable, and is to lexigraphically come first - even
%%%   before the empty string;
%%% - lexigraphic ordering needs to work on tuples too.
%%%
%%% The encoding has the invariant that an encoded string
%%% - contains no 0-bytes (because this value is used as tuple element
%%% separator;
%%% - contains 1-bytes only as escape values.
%%%
%%% The encoding is as follows:
%%% - the empty string is encoded as <<1,1>>.
%%% - <<0>> is encoded as <<1,2>>.
%%% - <<1>> is encoded as <<1,3>>.
%%% - all other bytes as encoded as-are.

%%% Slower versions:
% encode_string(null) -> <<>>;
% encode_string(<<>>) -> <<1,1>>;
% encode_string(V) ->
%     V2 = binary:replace(V,  <<1>>, <<1,3>>, [global]),
%     V3 = binary:replace(V2, <<0>>, <<1,2>>, [global]),
%     V3.
% 
% encode_string2(null) -> <<>>;
% encode_string2(<<>>) -> <<1,1>>;
% encode_string2(V) ->
%     << << (case X of 0 -> <<1,2>>; 1 -> <<1,3>>; _ -> <<X>> end)/binary>> || <<X>> <= V>>.

encode_string(null) -> <<>>;
encode_string(<<>>) -> <<1,1>>;
encode_string(V) -> encode_string(V, <<>>).

encode_string(<<>>, Acc) -> Acc;
encode_string(<<0, Rest/binary>>, Acc) when is_binary(Acc) ->
    encode_string(Rest, <<Acc/binary, 1, 2>>);
encode_string(<<1, Rest/binary>>, Acc) when is_binary(Acc) ->
    encode_string(Rest, <<Acc/binary, 1, 3>>);
encode_string(<<X, Rest/binary>>, Acc) when is_binary(Acc) ->
    encode_string(Rest, <<Acc/binary, X>>).

decode_string(<<>>) -> {null, <<>>};
decode_string(<<0, _/binary>>=Rest) -> {null, Rest};
decode_string(<<1,1, Rest/binary>>) -> {<<>>, Rest};
decode_string(V) ->
    case binary:split(V, <<0>>) of
        [V1]       -> Rest = <<>>;
        [V1,Rest1] -> Rest = <<0, Rest1/binary>>
    end,
    {decode_string_loop(V1, <<>>), Rest}.

decode_string_loop(<<>>, Acc) ->
    Acc;
decode_string_loop(V, Acc) when is_binary(Acc) ->
    case binary:split(V, <<1>>) of
        [V1] -> <<Acc/binary, V1/binary>>;
        [V1, <<2,Rest/binary>>] -> decode_string_loop(Rest, <<Acc/binary, V1/binary, 0>>);
        [V1, <<3,Rest/binary>>] -> decode_string_loop(Rest, <<Acc/binary, V1/binary, 1>>)
    end.

%%% Slower versions:

%decode_string1(<<>>) -> {null, <<>>};
%decode_string1(<<0, _/binary>>=Rest) -> {null, Rest};
%decode_string1(<<1,1, Rest/binary>>) -> {<<>>, Rest};
%decode_string1(V) ->
%    case binary:split(V, <<0>>) of
%        [V1]       -> Rest = <<>>;
%        [V1,Rest1] -> Rest = <<0, Rest1/binary>>
%    end,
%    V2 = binary:replace(V1, <<1,2>>, <<0>>, [global]),
%    V3 = binary:replace(V2, <<1,3>>, <<1>>, [global]),
%    {V3, Rest}.
%
% decode_string2(<<>>) -> {null, <<>>};
% decode_string2(<<0, _/binary>>=Rest) -> {null, Rest};
% decode_string2(<<1,1, Rest/binary>>) -> {<<>>, Rest};
% decode_string2(V) -> decode_string2(V, <<>>).
%
% decode_string2(<<>>=V, Acc) when is_binary(Acc) ->
%     {Acc, V};
% decode_string2(<<0, _/binary>>=V, Acc) when is_binary(Acc) ->
%     {Acc, V};
% decode_string2(<<1,2, Rest/binary>>, Acc) when is_binary(Acc) ->
%     decode_string2(Rest, <<Acc/binary, 0>>);
% decode_string2(<<1,3, Rest/binary>>, Acc) when is_binary(Acc) ->
%     decode_string2(Rest, <<Acc/binary, 1>>);
% decode_string2(<<1,Rest/binary>>, Acc) when is_binary(Acc) ->
%     error({bad_encoded_string_escape, Rest});
% decode_string2(<<X,Rest/binary>>, Acc) when is_binary(Acc) ->
%     decode_string2(Rest, <<Acc/binary, X>>).

