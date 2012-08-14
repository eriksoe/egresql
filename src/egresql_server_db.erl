%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Database file manager
%%%
%%%
-module(egresql_server_db).
-behaviour(gen_server).

%% API
-export([start_link/1, get/2, put/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
          db :: hanoidb:hanoidb()
         }).
-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
start_link(Filename) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {Filename}, []).

get(DB, Key) when is_pid(DB), is_binary(Key) ->
    gen_server:call(DB, {get, Key}).

put(DB, Key, Value) when is_pid(DB), is_binary(Key), is_binary(Value) ->
    gen_server:call(DB, {put, Key, Value}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init({Filename}) ->
    try hanoidb:open(Filename) of
        {ok, DB} ->
            {ok, #state{db = DB}}
    catch _:Reason ->
            error_logger:error_msg("~s: Could not open database ~s: ~p\n** Trace: ~p\n",
                                   [?MODULE, Filename, Reason, erlang:get_stacktrace()]),
            {error, {could_not_open_database, Reason}}
    end.

handle_call({get, Key}, From, State=#state{db=DB}) ->
    error_logger:info_msg("~s: get: DB=~p\n", [?MODULE, DB]),
    {reply, hanoidb:get(DB, Key), State};
handle_call({put, Key, Value}, From, State=#state{db=DB}) ->
    {reply, hanoidb:put(DB, Key, Value), State};
handle_call(Request, From, State) ->
    error_logger:error_msg("~s: Got unexpected call from ~p: ~p\n", [?MODULE, From, Request]),
    {noreply, State}.

handle_cast(Msg, State) ->
    error_logger:error_msg("~s: Got unexpected cast: ~p\n", [?MODULE, Msg]),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

