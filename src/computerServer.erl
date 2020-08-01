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
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-define(N, 20). % number of processes in all the program "Robins"
-define(DemilitarizedZone, 50). % how much area to add to each computer, "Demilitarized zone".


-record(computerStateM_state, {computerNodes,computersArea, myArea}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(List::list()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link([ComputerNodes,ComputersArea]) ->
  gen_server:start_link({global, node()}, ?MODULE, [ComputerNodes,ComputersArea], []).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #computerStateM_state{}} | {ok, State :: #computerStateM_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([ComputerNodes,ComputersArea]) -> %gets the area of the computer, the borders.
  ets:new(etsX,[ordered_set,public,named_table,{read_concurrency, true},{write_concurrency, true}]),
  ets:new(etsY,[ordered_set,public,named_table,{read_concurrency, true},{write_concurrency, true}]),
  MyArea = getArea(ComputerNodes,ComputersArea,node()),
  initRobins(MyArea),   %% spawn N/4 Robins
  {ok, #computerStateM_state{computerNodes = ComputerNodes, computersArea = ComputersArea, myArea = MyArea}}. %saves the area border

% gets My Area from lists of nodes and areas
getArea([],[],_)-> areaCantBeFound;
getArea([MyNode|_],[Area|_],MyNode) -> Area;
getArea([_|T1],[_|T2],MyNode) -> getArea(T1,T2,MyNode).

initRobins(MyArea) -> %spawn N/4 Robins
  Loop = lists:seq(1,?N div 4),
  [erlang:monitor(process,spawn(moveSimulator,start_link,[MyArea]),true)|| _<- Loop]. %spawn a Robin and monitor it

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
handle_call(sendLocations, _From, State = #computerStateM_state{}) ->
  {reply, {ets:tab2list(etsX),ets:tab2list(etsY)}, State};

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
