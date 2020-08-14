%%%-------------------------------------------------------------------
%%% @author amit
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Aug 2020 12:36 PM
%%%-------------------------------------------------------------------
-module(moveSimulator).
-author("amit").

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3,castPlease/1]).

-define(SERVER, ?MODULE).

-record(moveSimulator_state, {myArea}).


%%test TODO delete
castPlease(MSG)-> gen_server:cast({global, tal@ubuntu},{test,MSG}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(Area::term()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link([Area]) ->
  gen_server:start_link( ?MODULE, [Area], []), %TODO change the name ?MODULE, it wont work with more then 1
  receive
  after 2000-> ok
  end,
  castPlease(moveSimulatorOnline).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #moveSimulator_state{}} | {ok, State :: #moveSimulator_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([MyArea]) ->
  batmanProtocol:start_link(),
   %spawn_link(batmanProtocol,start_link,[]),  %creates batmanProtocol and link it to this process
  {ok, #moveSimulator_state{myArea = MyArea}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #moveSimulator_state{}) ->
  {reply, Reply :: term(), NewState :: #moveSimulator_state{}} |
  {reply, Reply :: term(), NewState :: #moveSimulator_state{}, timeout() | hibernate} |
  {noreply, NewState :: #moveSimulator_state{}} |
  {noreply, NewState :: #moveSimulator_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #moveSimulator_state{}} |
  {stop, Reason :: term(), NewState :: #moveSimulator_state{}}).
handle_call(_Request, _From, State = #moveSimulator_state{}) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #moveSimulator_state{}) ->
  {noreply, NewState :: #moveSimulator_state{}} |
  {noreply, NewState :: #moveSimulator_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #moveSimulator_state{}}).
% if i was close to the border, but the border changed, the computerServer will update us by sending
% updateArea cast msg, as following:
handle_cast({updateArea,NewArea}, _) ->
  {noreply, #moveSimulator_state{myArea = NewArea}};
handle_cast(_Request, State = #moveSimulator_state{}) ->
  {noreply, State}.


%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #moveSimulator_state{}) ->
  {noreply, NewState :: #moveSimulator_state{}} |
  {noreply, NewState :: #moveSimulator_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #moveSimulator_state{}}).
handle_info(_Info, State = #moveSimulator_state{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #moveSimulator_state{}) -> term()).
terminate(_Reason, _State = #moveSimulator_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #moveSimulator_state{},
    Extra :: term()) ->
  {ok, NewState :: #moveSimulator_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #moveSimulator_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
