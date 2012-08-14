-module(egresql_server_auth).
-export([authenticate_by_md5/4,
        admin_create_md5_auth/3]).

admin_create_md5_auth(DatabaseName, UserName, Password) ->
    {ok, DB} = egresql_server_dbreg:get_database(DatabaseName),
    Digest = binary_to_hex(crypto:md5(<<Password/binary, UserName/binary>>)),
    egresql_server_db:put(DB, <<"Amd5:", UserName/binary>>, Digest).


-spec(authenticate_by_md5/4 :: (binary(), binary(), binary(), binary()) ->
                                    {ok, pid()} | false).
authenticate_by_md5(DatabaseName, UserName, Salt, ClientPwdHash)
  when is_binary(DatabaseName),
       is_binary(UserName),
       is_binary(Salt),
       is_binary(ClientPwdHash) ->
    Result = do_authenticate_by_md5(DatabaseName, UserName, Salt, ClientPwdHash),
    case Result of
        {ok, DB} ->
            error_logger:info_msg("Authentication succeeded for (database=~s, user=~s)\n",
                                  [DatabaseName, UserName]);
        {error, Reason} ->
            error_logger:warning_msg("Authentication failed: ~p\n(database=~s, user=~s)\n",
                                     [Reason, DatabaseName, UserName])
    end,
    Result.

do_authenticate_by_md5(DatabaseName, UserName, Salt, ClientPwdHash) ->
    case egresql_server_dbreg:get_database(DatabaseName) of
        {error, Reason} -> {error, Reason};
        {ok, DB} ->
            error_logger:info_msg("~s: DB=~p\n", [?MODULE, DB]),
            %% Look up authentication information:
            case egresql_server_db:get(DB, <<"Amd5:", UserName/binary>>) of
                not_found -> {error, bad_credentials};
                {ok, PwdUserHash} ->
                    Expected = calc_md5_hash(PwdUserHash, Salt),
                    if ClientPwdHash =:= Expected ->
                            {ok, DB};
                       true ->
                            {error, bad_credentials}
                    end
            end
    end.


%%%========== Hash functions ===================================
calc_md5_hash(PwdUserHash, Salt) ->
    %Digest1 = crypto:md5(<<Password/binary, Username/binary>>),
    %Digest1Txt = binary_to_hex(Digest1),
    Digest1Txt = PwdUserHash,
    Digest2 = crypto:md5(<<Digest1Txt/binary, Salt/binary>>),
    binary_to_hex(Digest2).

binary_to_hex(Bin) ->
    << <<(hexchar(Hi)), (hexchar(Lo))>> || <<Hi:4, Lo:4>> <= Bin>>.

hexchar(X) when X < 10 -> $0 + X;
hexchar(X) -> $a + (X - 10).
