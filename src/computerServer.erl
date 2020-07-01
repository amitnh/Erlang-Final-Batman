%%%-------------------------------------------------------------------
%%% @author amit
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Jun 2020 6:42 PM
%%%-------------------------------------------------------------------
-module(computerServer).
-author("amit").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(computerStateM_state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #computerStateM_state{}} | {ok, State :: #computerStateM_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  ets:new(etsX,[ordered_set,public,named_table]),
  ets:new(etsY,[ordered_set,public,named_table]),
  gen_server:cast(mainServer,hello),
  {ok, #computerStateM_state{}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #computerStateM_state{}) ->
  {reply, Reply :: term(), NewState :: #computerStateM_state{}} |
  {reply, Reply :: term(), NewState :: #computerStateM_state{}, timeout() | hibernate} |
  {noreply, NewState :: #computerStateM_state{}} |
  {noreply, NewState :: #computerStateM_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #computerStateM_state{}} |
  {stop, Reason :: term(), NewState :: #computerStateM_state{}}).
handle_call(_Request, _From, State = #computerStateM_state{}) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #computerStateM_state{}) ->
  {noreply, NewState :: #computerStateM_state{}} |
  {noreply, NewState :: #computerStateM_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #computerStateM_state{}}).
handle_cast(_Request, State = #computerStateM_state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #computerStateM_state{}) ->
  {noreply, NewState :: #computerStateM_state{}} |
  {noreply, NewState :: #computerStateM_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #computerStateM_state{}}).
handle_info(_Info, State = #computerStateM_state{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #computerStateM_state{}) -> term()).
terminate(_Reason, _State = #computerStateM_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #computerStateM_state{},
    Extra :: term()) ->
  {ok, NewState :: #computerStateM_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #computerStateM_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
