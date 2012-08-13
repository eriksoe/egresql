-ifndef(egres_wire_protocol).
-define(egres_wire_protocol, yes).

%%% Message types:
-define(PG_MSGTYPE_B, $B).
-define(PG_MSGTYPE_D, $D).
-define(PG_MSGTYPE_ERROR, $E).                  % Server -> client
-define(PG_MSGTYPE_F, $F).

-define(PG_MSGTYPE_P, $P).
-define(PG_MSGTYPE_QUERY, $Q).                  % Client -> server
-define(PG_MSGTYPE_AUTHREQUEST, $R).            % Server -> client
-define(PG_MSGTYPE_S, $S).
-define(PG_MSGTYPE_X, $X).

-define(PG_MSGTYPE_c, $c).
-define(PG_MSGTYPE_d, $d).
-define(PG_MSGTYPE_f, $f).
-define(PG_MSGTYPE_PASSWORD, $p).               % Client -> server

%% Backend -> frontend:
%% V: Function result
%% E: Error return
%% A: Notify
%% N: Notice
%% Z: Ready for new query
%% S: Parameter status
%% C: Command complete
%% I: Empty query
%% 1,2,3: Parse,bind,close complete
%% K: Secret key
%% T: Row decription
%% n: No data
%% t: Parameter description
%% D: Data row
-define(PG_MSGTYPE_SAVE_PARAMETER, $S).
-define(PG_MSGTYPE_READY_FOR_QUERY, $Z).
-define(PG_MSGTYPE_RESULT_VALUE, $V).


-define(PG_AUTHREQ_OK, 0).
-define(PG_AUTHREQ_MD5, 5).

-define(PG_XACTSTATUS_IDLE, $I).
-define(PG_XACTSTATUS_INTRANS, $T).
-define(PG_XACTSTATUS_INERROR, $E).

%%% Types:
-define(PG_TYPE_BOOL,       16).
-define(PG_TYPE_BYTEA,      17).
-define(PG_TYPE_CHAR,       18).
-define(PG_TYPE_NAME,       19).
-define(PG_TYPE_INT8,       20).
-define(PG_TYPE_INT2,       21).
-define(PG_TYPE_INT2VECTOR, 22).
-define(PG_TYPE_INT4,       23).
-define(PG_TYPE_REGPROC,    24).
-define(PG_TYPE_TEXT,       25).
-define(PG_TYPE_OID,        26).

-endif.
