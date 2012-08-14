%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Database registry
%%%
%%%
-module(egresql_server_dbreg).
-behaviour(gen_server).

-include_lib("kernel/include/file.hrl").
%% API
-export([start_link/0]).
-export([get_database/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
          dbtable :: ets:tid()
         }).
-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get_database(DBName) when is_binary(DBName) ->
    gen_server:call(?SERVER, {get_database, DBName}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    %% TODO: process_flag(trap_exit, true) + handle signals.
    {ok, #state{
       dbtable = ets:new(egresql_db_register, [protected])
      }}.

handle_call({get_database, DBName}, _From, State=#state{dbtable=Table}) ->
    case ets:lookup(Table, DBName) of
        [{DBName, Pid}] ->
            Result = {ok,Pid};
        [] ->
            %% That database is not yet known. Try to open it:
            Result = get_new_database(DBName, State)
    end,
    {reply, Result, State};
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

get_new_database(DBName, State) when is_binary(DBName) ->
    %% Step 1: Syntactic check.
    case is_valid_database_name(DBName) of
        false -> {error, invalid_db_name};
        true ->
            %% Step 2: Superficial feasibility check.
            Filename = binary_to_list(DBName) ++ ".egres",
            case check_file_accessibility(Filename) of
                {error, _}=Err -> Err;
                ok ->
                    %% Step 3: Try for real.
                    case egresql_server_db:start_link(Filename) of
                        {error, _}=Err -> Err;
                        {ok, Pid}      ->
                            ets:insert(State#state.dbtable, {DBName,Pid}),
                            {ok, Pid}
                    end
            end
    end.

%% Check that the database name is a safe file name:
is_valid_database_name(DBName) ->
    case re:run(DBName, "^[A-Za-z0-9_]+$") of %") of
        {match, _} -> true;
        nomatch    -> false
    end.

check_file_accessibility(Filename) ->
    case file:read_file_info(Filename) of
        {ok, #file_info{access=X}} when X/=read_write -> {error, eaccess};
        {ok, #file_info{type=X}}   when X/=directory  -> {error, enotdir};
        {ok, #file_info{}} -> ok;
        {error, _}=Err -> Err
    end.

