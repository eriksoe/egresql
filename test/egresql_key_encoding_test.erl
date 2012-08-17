-module(egresql_key_encoding_test).
-include_lib("eunit/include/eunit.hrl").
-undef(LET).
-include_lib("triq/include/triq.hrl").

-compile(export_all).

int_encode_decode_test() -> ?assert(triq:check(prop_int_encode_decode(), 10000)).
int_encode_order_test()  -> ?assert(triq:check(prop_int_encode_order(),  20000)).
int_encode_tail_test()  -> ?assert(triq:check(prop_int_encode_tail(), 1000)).

string_encode_decode_test() -> ?assert(triq:check(prop_string_encode_decode(), 2000)).
string_encode_order_test()  -> ?assert(triq:check(prop_string_encode_order(),  2000)).
string_encode_tail_test()  -> ?assert(triq:check(prop_string_encode_tail(), 2000)).

%%%======================================================================
prop_int_encode_decode() ->
    ?FORALL(X, integer_or_null(),
         begin
             egresql_key_encoding:decode(integer,
               egresql_key_encoding:encode(integer, X))
                 == {X, <<>>}
         end).

prop_int_encode_order() ->
    ?FORALL({X,Y}, {integer_or_null(), integer_or_null()},
            expected_int_order(X,Y) =:=
            binary_order(egresql_key_encoding:encode(integer, X),
                         egresql_key_encoding:encode(integer, Y))).

prop_int_encode_tail() ->
    ?FORALL({X,T}, {integer_or_null(), binary()},
            egresql_key_encoding:decode(integer,
               <<(egresql_key_encoding:encode(integer, X))/binary,
                 T/binary>>)
            =:= {X, T}).


%%%----------------------------------------
prop_string_encode_decode() ->
    ?FORALL(X, string_or_null(),
         begin
             egresql_key_encoding:decode(string,
               egresql_key_encoding:encode(string, X))
                 == {X, <<>>}
         end).

prop_string_encode_order() ->
    ?FORALL({X,Y}, {string_or_null(), string_or_null()},
            expected_string_order(X,Y) =:=
            binary_order(egresql_key_encoding:encode(string, X),
                         egresql_key_encoding:encode(string, Y))).

prop_string_encode_tail() ->
    ?FORALL({X,T}, {string_or_null(), binary()},
            egresql_key_encoding:decode(string,
               <<(egresql_key_encoding:encode(string, X))/binary,
                 0, T/binary>>)
            =:= {X, <<0, T/binary>>}).

%%======================================================================

expected_int_order(X, X) -> eq;
expected_int_order(null, _) -> lt;
expected_int_order(_, null) -> gt;
expected_int_order(X, Y) when X<Y -> lt;
expected_int_order(X, Y) when X>Y -> gt.

expected_string_order(X, X) -> eq;
expected_string_order(null, _) -> lt;
expected_string_order(_, null) -> gt;
expected_string_order(X, Y) when X<Y -> lt;
expected_string_order(X, Y) when X>Y -> gt.

binary_order(X,X) when is_binary(X) -> eq;
binary_order(X,Y) when is_binary(X), is_binary(Y), X<Y -> lt;
binary_order(X,Y) when is_binary(X), is_binary(Y), X>Y -> gt.



integer_or_null() ->
    frequency([{1,null}, {40,integer()}]).

integer() ->
    ?SUCHTHAT(N,
              ?LET({X,Y,Z}, {choose(-1000, 1000), choose(-1000, 1000), choose(0,24)},
                   X + (Y bsl Z)),
              begin
                  %% N can be represented in 32bits signed:
                  <<M:32/signed>> = <<N:32/signed>>,
                  M=:=N
              end).


string_or_null() ->
    frequency([{1,null}, {40,binary()}]).


%%%======================================================================

integer_encoding_speed_test() ->
    encoding_speed_test_aux(integer_or_null(), integer).

string_encoding_speed_test()  ->
    encoding_speed_test_aux(string_or_null(), string).

encoding_speed_test_aux(Gen, Type) ->
    N = 25000,
    Inputs = [null] ++ [element(2,triq_dom:pick(Gen, 25)) || _ <- lists:seq(1,N)],
    T1 = os:timestamp(),
    Encoded = lists:map(fun (V) -> egresql_key_encoding:encode(Type,V) end,
                        Inputs),
    T2 = os:timestamp(),
    _Decoded = lists:map(fun (V) -> egresql_key_encoding:decode(Type,V) end,
                        Encoded),
    T3 = os:timestamp(),
    io:format(user, "encoding_speed_test for type ~s: encoding ~.1f/s, decoding ~.1f/s\n",
              [Type,
               (N*1.0e6) / timer:now_diff(T2,T1),
               (N*1.0e6) / timer:now_diff(T3,T2)]),
    ok.
