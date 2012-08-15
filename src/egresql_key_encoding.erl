-module(egresql_key_encoding).
-export([encode/2, decode/2]).

encode(integer, V) -> encode_int(V).

decode(integer, Bin) -> decode_int(Bin).


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


decode_int(<<0,           Rest/binary>>) -> {null, Rest};
decode_int(<<16#7C, N:32/unsigned, Rest/binary>>) -> {N - (1 bsl 32), Rest};
decode_int(<<16#7D, N:24/unsigned, Rest/binary>>) -> {N - (1 bsl 24), Rest};
decode_int(<<16#7E, N:16/unsigned, Rest/binary>>) -> {N - (1 bsl 16), Rest};
decode_int(<<16#7F, N:8 /unsigned, Rest/binary>>) -> {N - (1 bsl  8), Rest};
decode_int(<<16#80,                Rest/binary>>) -> {0, Rest};
decode_int(<<16#81, N:8 /unsigned, Rest/binary>>) -> {N, Rest};
decode_int(<<16#82, N:16/unsigned, Rest/binary>>) -> {N, Rest};
decode_int(<<16#83, N:24/unsigned, Rest/binary>>) -> {N, Rest};
decode_int(<<16#84, N:32/unsigned, Rest/binary>>) -> {N, Rest}.
