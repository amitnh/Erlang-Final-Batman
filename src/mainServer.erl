%%%-------------------------------------------------------------------
%%% @author amit
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Jun 2020 3:48 PM
%%%-------------------------------------------------------------------
%%% C5 in charge of spawning the other computers and monitor them
%%%-------------------------------------------------------------------
%%%-------------------------------------------------------------------

-module(mainServer).
-author("amit").

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(mainServer_state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(ComputerNodes::list(),ComputersArea::list()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
% ComputerNodes-> [tal@ubuntu,yossi@megatron....], size 4
% ComputersArea-> [{startX,endX,startY,endY},...] size 4
start_link(ComputerNodes,ComputersArea) ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, [{ComputerNodes,ComputersArea}], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args::term()) ->
  {ok, State :: #mainServer_state{}} | {ok, State :: #mainServer_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).

init([{ComputerNodes,ComputersArea}]) ->
  io:format("1~n"),
  ets:new(etsRobins,[set,named_table,public]), % Pid@Node -> {X,Y}
  %lists:zipwith(fun(Atom,Node) -> put(Atom,Node) end, [c1,c2,c3,c4], ComputerNodes), % saves the Nodes of the computers
  %lists:zipwith(fun(Atom,Area) -> put(Atom,Area) end, [area1,area2,area3,area4], ComputersArea), % saves the Nodes area
  io:format("2~n"),
  spawnComputer(ComputerNodes,ComputersArea,loop),
  io:format("3~n"),
  {ok, #mainServer_state{}}.

%start server for computer for each node in the ComputerNodes list
spawnComputer(ComputerNodes,ComputersArea,loop) -> [spawnComputer(ComputerNodes,ComputersArea,Node) || Node<- ComputerNodes];
% spawns a Computer at a specific node and monitors it
spawnComputer(ComputerNodes,ComputersArea,Node) ->   rpc:call(Node, computerServer,start_link,[ComputerNodes,ComputersArea]), %builds a Computer at Node
  erlang:monitor_node(Node,true). % makes the mainServer monitor the new computer at Node



%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #mainServer_state{}) ->
  {reply, Reply :: term(), NewState :: #mainServer_state{}} |
  {reply, Reply :: term(), NewState :: #mainServer_state{}, timeout() | hibernate} |
  {noreply, NewState :: #mainServer_state{}} |
  {noreply, NewState :: #mainServer_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #mainServer_state{}} |
  {stop, Reason :: term(), NewState :: #mainServer_state{}}).
handle_call(_Request, _From, State = #mainServer_state{}) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #mainServer_state{}) ->
  {noreply, NewState :: #mainServer_state{}} |
  {noreply, NewState :: #mainServer_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #mainServer_state{}}).
handle_cast(hello, State = #mainServer_state{}) ->
  io:format("hello"),
  {noreply, State};
handle_cast(_Request, State = #mainServer_state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #mainServer_state{}) ->
  {noreply, NewState :: #mainServer_state{}} |
  {noreply, NewState :: #mainServer_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #mainServer_state{}}).
handle_info(_Info, State = #mainServer_state{}) ->
  {noreply, State}.

%%handle_info({'EXIT', Pid, Reason}, State) ->
%%  ..code to handle exits here..
%%  {noreply, State1}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #mainServer_state{}) -> term()).
terminate(_Reason, _State = #mainServer_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #mainServer_state{},
    Extra :: term()) ->
  {ok, NewState :: #mainServer_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #mainServer_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
