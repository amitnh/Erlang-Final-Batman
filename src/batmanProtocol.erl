%%%-------------------------------------------------------------------
%%% @author amit
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Jul 2020 12:48 PM
%%%-------------------------------------------------------------------
-module(batmanProtocol).
-author("amit").

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).

%%%===================================================================
-record(batmanProtocol_state, {known,pid,seqNum}).
%% pid - the pid of my moveSimulator for putting it inside OGM / MSG
%% -------------------------------------------------------
%% known is a map of known Robins in the system
%% each Robin contains map of neighbors Pid that he received messages from:
%% known ->                {key-> Pid@Node, Value -> {Current Seq Number, Best Link, Last Aware Time, list of neighbors}}
%% list of neighbors ->    {sorted-list of in window seq numbers, last TTL, last Valid Time}
%%%===================================================================
%% OGM -> {Sequence number, TTL, Originator Address }
%% Originator Address -> {self(),node()}
%%%===================================================================
-define(windowSize, 128). %% Define the size of the window. only sequence numbers in the window will be counted and saved
-define(TTL, 40). %% Time To Live, travel distance of a message
-define(ORIGINATOR_INTERVAL, 1000). %% every ORIGINATOR_INTERVAL the node sends an OGM msg
%%test TODO delete
castPlease(MSG)-> gen_server:cast({global, tal@ubuntu},{test,MSG}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(PidMoveSimulator:: term()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(PidMoveSimulator) ->
  {ok,Pid} = gen_server:start_link(?MODULE, [PidMoveSimulator], [{debug,[trace]}]),
  spawn_link(fun()->ogmLoop(Pid) end). %sends cast to OGM every ORIGINATOR_INTERVAL


ogmLoop(Pid)-> % sends OGM cast to batmanProtocol to send OGM every ?ORIGINATOR_INTERVAL time
  receive
  after ?ORIGINATOR_INTERVAL -> gen_server:cast(Pid,{sendOGM})
  end,
  ogmLoop(Pid).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #batmanProtocol_state{}} | {ok, State :: #batmanProtocol_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([PidMoveSimulator]) ->
  {ok, #batmanProtocol_state{known = maps:new(),pid = PidMoveSimulator,seqNum=0}}. %return a new empty map of known Robins


%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #batmanProtocol_state{}) ->
  {reply, Reply :: term(), NewState :: #batmanProtocol_state{}} |
  {reply, Reply :: term(), NewState :: #batmanProtocol_state{}, timeout() | hibernate} |
  {noreply, NewState :: #batmanProtocol_state{}} |
  {noreply, NewState :: #batmanProtocol_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #batmanProtocol_state{}} |
  {stop, Reason :: term(), NewState :: #batmanProtocol_state{}}).
handle_call(_Request, _From, State = #batmanProtocol_state{}) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #batmanProtocol_state{}) ->
  {noreply, NewState :: #batmanProtocol_state{}} |
  {noreply, NewState :: #batmanProtocol_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #batmanProtocol_state{}}).

handle_cast({sendOGM}, State = #batmanProtocol_state{}) ->
  SeqNum = State#batmanProtocol_state.seqNum +1,
  OGM = {SeqNum, ?TTL,{State#batmanProtocol_state.pid,node()}},
  gen_server:cast(State#batmanProtocol_state.pid,{sendToNeighbors,OGM}),
  {noreply, State#batmanProtocol_state{seqNum = SeqNum}};

handle_cast(_Request, State = #batmanProtocol_state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #batmanProtocol_state{}) ->
  {noreply, NewState :: #batmanProtocol_state{}} |
  {noreply, NewState :: #batmanProtocol_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #batmanProtocol_state{}}).
handle_info(_Info, State = #batmanProtocol_state{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #batmanProtocol_state{}) -> term()).
terminate(_Reason, _State = #batmanProtocol_state{}) ->
  ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================
