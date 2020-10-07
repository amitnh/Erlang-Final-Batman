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
-define(UpdateTime, 1000). % time for sending the ETSES tables
-define(LineFrames, 80). %number of frames to show the line

-record(mainServer_state, {computerNodes,computerAreas}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(ComputerNodes::list(),ComputerAreas::list()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
% ComputerNodes-> [tal@ubuntu,yossi@megatron....], size 4
% ComputerAreas-> [{startX,endX,startY,endY},...] size 4
start_link(ComputerNodes,ComputerAreas) ->
    gen_server:start_link({global, node()}, ?MODULE, [{ComputerNodes,ComputerAreas}],[]),% [{debug,[trace]}]). %TODO delete trace
    guiStateM:start_link(ComputerNodes).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args::term()) ->
  {ok, State :: #mainServer_state{}} | {ok, State :: #mainServer_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).

init([{ComputerNodes,ComputerAreas}]) ->
  % etsRobins: {Pid,Node} -> {X,Y}, {{<0.112.0>,tal@ubuntu},X,Y}
  ets:new(etsRobins,[set,public,named_table]),
  ets:new(etsMsgs,[ordered_set,public,named_table,{read_concurrency, true},{write_concurrency, true}]),

%%  lists:zipwith(fun(Atom,Node) -> put(Atom,Node) end, [c1,c2,c3,c4], ComputerNodes), % saves the Nodes of the computers todo
%%  lists:zipwith(fun(Atom,Area) -> put(Atom,Area) end, [area1,area2,area3,area4], ComputerAreas), % saves the Nodes area todo
  spawn_link(fun()->monitorComputers(ComputerNodes,node()) end),
  spawnComputer(ComputerNodes,ComputerAreas,loop),
  spawn_link(fun()->testMsgSending() end),

  {ok, #mainServer_state{computerNodes = ComputerNodes,computerAreas = ComputerAreas}}.
testMsgSending()->

  receive after 4000  ->
  First = ets:first(etsRobins),
    {PidFrom,NodeFrom} = takeNelement(First,First,rand:uniform(20)),

    {PidTo,NodeTo} = gen_server:call({global,NodeFrom},{getKnownFrom, PidFrom}),
    if(PidTo == notfound ) -> testMsgSending();
%%    {PidTo,NodeTo} = takeNelement(First,First,rand:uniform(20)),
%%    if NodeFrom == NodeTo -> testMsgSending();
      true->
              spawn(NodeFrom,gen_server,cast,[PidFrom,{sendMsg,{PidTo,NodeTo},{PidFrom,NodeFrom},helloBanana}])
        , testMsgSending()
    end
    end.

takeNelement('$end_of_table',Xlast, _) -> Xlast;
takeNelement(X,_Xlast, 0) -> X;
takeNelement(X,_Xlast, N) ->
  takeNelement(ets:next(etsRobins,X),X, N-1).

%a process updateMainServer sends every UpdateTime mili secs the ETS tables to the main server

%start server for computer for each node in the ComputerNodes list
spawnComputer(ComputerNodes,ComputerAreas,loop) -> [spawnComputer(ComputerNodes,ComputerAreas,Node) || Node<- ComputerNodes];
% spawns a Computer at a specific node and monitors it
spawnComputer(ComputerNodes,ComputerAreas,Node) ->

  spawn(Node,computerServer,start_link,[[ComputerNodes,ComputerAreas,node()]]).

%%==================================================================================
%%monitors all computer Servers(move simulators)
%%==================================================================================
monitorComputers(ComputerNodes,MainServerNode) ->
  [erlang:monitor_node(Node,true)||Node<-ComputerNodes], % monitors all the nodes/computerServers
  monitorComputersReceieveLoop(MainServerNode).

monitorComputersReceieveLoop(MainServerNode)->
  receive
    {nodedown,Node} -> gen_server:cast({global,MainServerNode},{nodedown, Node});
    SomeError ->   moveSimulator:castPlease({someError,SomeError})
  end,
  monitorComputersReceieveLoop(MainServerNode).
%%==================================================================================

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
  moveSimulator:castPlease({missedCallmainServer, request, _Request, from, _From}),

  {reply, ok, State}.





%takes a list of pids from ETSX or ETSY, and updates their location in the ETSROBINS
updateEts(_,[],_,_)-> ok;
updateEts(Location,[Pid|Pids],XorY,Node)-> IsMember = ets:member(etsRobins,{Pid,Node}),
  if  IsMember -> [{_FromTuple,{X,Y}}] = ets:lookup(etsRobins,{Pid,Node}), %if Robins already a member
      if XorY == x -> ets:insert(etsRobins,{{Pid,Node},{Location,Y}});
        true -> ets:insert(etsRobins,{{Pid,Node},{X,Location}})
      end;
      true-> ets:insert(etsRobins,{{Pid,Node},{Location,Location}}) %if Robins is not a member, he is new
    end,
  updateEts(Location,Pids,XorY,Node). %recursion call



%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #mainServer_state{}) ->
  {noreply, NewState :: #mainServer_state{}} |
  {noreply, NewState :: #mainServer_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #mainServer_state{}}).



handle_cast({test,M}, State = #mainServer_state{}) ->
  io:format("mainServer got this MSG: ~p~n",[M]),
  {noreply, State};
%regular ETS update from Node
%EtsX and EtsY are lists of the original ETSes
handle_cast({etsUpdate,From,EtsX,EtsY}, State = #mainServer_state{}) ->
  spawn(fun()-> [updateEts(X,PidList,x,From)||{X,PidList}<-EtsX],
    [updateEts(Y,PidList,y,From)||{Y,PidList}<-EtsY] end),
  {noreply, State};

%Removes a Robin from the ETSRobins
handle_cast({removeRobin,Pid,Node}, State = #mainServer_state{}) ->
  ets:delete(etsRobins, {Pid, Node}),
%%gen_server:cast({global, tal@ubuntu},{test,{removedPid,Pid,etsRobins,ets:tab2list(etsRobins),deleted,Bool}}),
  {noreply, State};

%Add a new message to etsMsgs
handle_cast({addMessage,From,To}, State = #mainServer_state{}) ->
  ets:insert(etsMsgs, {{From,To}, ?LineFrames}),
  {noreply, State};

handle_cast({nodedown, MyNode}, State = #mainServer_state{}) ->
  io:format("ive got node down and i cannot lie: ~p~n",[{nodedown,node, Node}]),
  % update boarders on all remaining computerServers
  % delete from EtsRobins and spawn new ones, and send the locations to transfer to the new computer
  % if 4 nodes -> computer to the right or to the left takes it and gets X,Y locations
  % if 3 nodes and sx = 0 and ex = 0 then count the nodes
  % if 2 nodes -> takes all and get the locations
  ComputerNodes = State#mainServer_state.computerNodes,
  ComputerAreas = State#mainServer_state.computerAreas,
  ZipLists = lists:zip(ComputerNodes,ComputerAreas),
  [{MySx,MyEx,MySy,MyEy}]= [Area||{Node,Area}<-ZipLists, Node == MyNode], % take the fallen node areas
  Size = length(ComputerNodes),
  NewComputerNodes = ComputerNodes -- [MyNode],

  if ((Size == 4) or ((Size == 3) and (MySx /= 0) or (MyEx /= 2000))) -> % case: take computer to the right or the left
      [{ChosenNode, {CSx,CEx,CSy,CEy}}] = [{Node, {Sx, Ex, Sy, Ey}} ||{Node,{Sx,Ex,Sy,Ey}}<- ZipLists, ((Ex == MySx) or (MyEx == Sx))],
      NewComputerAreas = ok, %todo
      [gen_server:cast({global,Node},{newBoarders,NewComputerNodes,NewComputerAreas})||Node<-NewComputerNodes], % send cast to change server boarders
      ListOfXY = [{X,Y}||{{_Pid,NodeRobin},{X,Y}}<-ets:tab2list(etsRobins),NodeRobin == MyNode ], % saves all the {X,Y} locations of the dead node
      gen_server:cast({global,ChosenNode},{newRobinsAtXY,ListOfXY}),
    (Size == 3) ->ok; % special case: take the other 2 nodes and exstend each of them and spawn new Robins
    (Size == 2) -> ok;%case:  the left node takes all and get the locations
    true-> io:format("everyone is dead, byebye"), gen_server:stop({global,node()})% no more nodes -> terminate
  end,
  {noreply, State#mainServer_state{computerNodes = NewComputerNodes, computerAreas = NewComputerAreas}};

handle_cast(_Request, State = #mainServer_state{}) ->
  moveSimulator:castPlease({missedCallMainSer, request, _Request}),

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
