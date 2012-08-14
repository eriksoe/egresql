%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Database file manager
%%%
%%%
-module(egresql_server_db).
-behaviour(gen_server).

%% API
-export([start_link/1]).

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


%%====================================================================
%% gen_server callbacks
%%====================================================================

init({Filename}) ->
    try hanoidb:open(Filename) of
        DB ->
            {ok, #state{db = DB}}
    catch _:Reason ->
            error_logger:error_msg("~s: Could not open database ~s: ~p\n** Trace: ~p\n",
                                   [?MODULE, Filename, Reason, erlang:get_stacktrace()]),
            {error, {could_not_open_database, Reason}}
    end.

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

