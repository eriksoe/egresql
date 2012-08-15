-module(egresql_key_encoding_test).
-include_lib("eunit/include/eunit.hrl").
-undef(LET).
-include_lib("triq/include/triq.hrl").

-compile(export_all).

encode_decode_test() -> ?assert(triq:check(prop_encode_decode(), 10000)).
encode_order_test()  -> ?assert(triq:check(prop_encode_order(),  20000)).

prop_encode_decode() ->
    ?FORALL(X, integer_or_null(),
         begin
             egresql_key_encoding:decode(integer,
               egresql_key_encoding:encode(integer, X))
                 == {X, <<>>}
         end).

prop_encode_order() ->
    ?FORALL({X,Y}, {integer_or_null(), integer_or_null()},
            expected_int_order(X,Y) =:=
            binary_order(egresql_key_encoding:encode(integer, X),
                         egresql_key_encoding:encode(integer, Y))).

expected_int_order(X, X) -> eq;
expected_int_order(null, _) -> lt;
expected_int_order(_, null) -> gt;
expected_int_order(X, Y) when X<Y -> lt;
expected_int_order(X, Y) when X>Y -> gt.

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



