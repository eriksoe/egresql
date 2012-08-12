-ifndef(egres_wire_protocol).
-define(egres_wire_protocol, yes).

%%% Message types:
-define(PG_MSGTYPE_B, $B).
-define(PG_MSGTYPE_D, $D).
-define(PG_MSGTYPE_ERROR, $E).
-define(PG_MSGTYPE_F, $F).

-define(PG_MSGTYPE_P, $P).
-define(PG_MSGTYPE_Q, $Q).
-define(PG_MSGTYPE_AUTHREQUEST, $R).
-define(PG_MSGTYPE_S, $S).
-define(PG_MSGTYPE_X, $X).

-define(PG_MSGTYPE_c, $c).
-define(PG_MSGTYPE_d, $d).
-define(PG_MSGTYPE_f, $f).
-define(PG_MSGTYPE_PASSWORD, $p).


-define(PG_AUTHREQ_OK, 0).
-define(PG_AUTHREQ_MD5, 5).


-endif.
