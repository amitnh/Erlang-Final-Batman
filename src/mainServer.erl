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
%%-define(UpdateTime, 1000). % time for sending the ETSES tables
-define(LineFrames, 80). %number of frames to show the line
-define(RefreshRate, 20).
% ComputerNodes-> [tal@ubuntu,yossi@megatron....], size 4
% ComputerAreas-> [{startX,endX,startY,endY},...] size 4
% processes -> [{node,numOfProcesses},{node,numOfProcesses}...]
% specs -> {Radius,NumofRobins, DemiZone,OGMTime,MaxVelocity,WindowSize,TTL},

-record(mainServer_state, {computerNodes,computerAreas,processes,specs}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(ComputerNodes::list(),ComputerAreas::list()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
% ComputerNodes-> [tal@ubuntu,yossi@megatron....], size 4
% ComputersArea-> [{startX,endX,startY,endY},...] size 4
start_link(ComputerNodes,ComputersArea) ->
    gen_server:start_link({global, node()}, ?MODULE, [ComputerNodes,ComputersArea],[]),% [{debug,[trace]}]). %TODO delete trace

  Pid= guiStateM:start_link([ComputerNodes, node()]),
    spawn_link(fun()-> refreshtimer(Pid) end).
% ComputerAreas-> [{startX,endX,startY,endY},...] size 4
%%start_link(ComputerNodes,ComputerAreas) ->
%%    gen_server:start_link({global, node()}, ?MODULE, [{ComputerNodes,ComputerAreas}],[]),% [{debug,[trace]}]). %TODO delete trace
%%    guiStateM:start_link(ComputerNodes).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args::term()) ->
  {ok, State :: #mainServer_state{}} | {ok, State :: #mainServer_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).

init([ComputerNodes, ComputerAreas]) ->
  % etsRobins: {Pid,Node} -> {X,Y}, {{<0.112.0>,tal@ubuntu},X,Y}
  ets:new(etsRobins,[set,public,named_table]),
  ets:new(etsMsgs,[ordered_set,public,named_table,{read_concurrency, true},{write_concurrency, true}]),

  spawn_link(fun()->monitorComputers(ComputerNodes,node()) end),
  % specs -> {Radius,NumofRobins, DemiZone,OGMTime,MaxVelocity,WindowSize,TTL},
  %start with default values
  spawnComputer(ComputerNodes,ComputerAreas, {300,100, 50,1000,10,128,30},loop),
  spawn_link(fun()->testMsgSending() end),

  {ok, #mainServer_state{processes = [], computerNodes = ComputerNodes,computerAreas = ComputerAreas, specs = {300,100, 50,1000,10,128,30}}}.
testMsgSending()->
 try
  receive after 2000  ->
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
    end
catch _:_ -> testMsgSending() end.

takeNelement('$end_of_table',Xlast, _) -> Xlast;
takeNelement(X,_Xlast, 0) -> X;
takeNelement(X,_Xlast, N) ->
  takeNelement(ets:next(etsRobins,X),X, N-1).

%a process updateMainServer sends every UpdateTime mili secs the ETS tables to the main server

%start server for computer for each node in the ComputerNodes list
spawnComputer(ComputerNodes,ComputerAreas,Specs,loop) -> [spawnComputer(ComputerNodes,ComputerAreas,Specs,Node) || Node<- ComputerNodes];
% spawns a Computer at a specific node and monitors it
spawnComputer(ComputerNodes,ComputerAreas,Specs,Node) ->

% specs -> {Radius,NumofRobins, DemiZone,OGMTime,MaxVelocity,WindowSize,TTL},
  spawn(Node,computerServer,start_link,[[ComputerNodes,ComputerAreas, Specs,node()]]).

%%==================================================================================
%%monitors all computer Servers(move simulators)
%%==================================================================================
monitorComputers(ComputerNodes,MainServerNode) ->
  [erlang:monitor_node(Node,true)||Node<-ComputerNodes], % monitors all the nodes/computerServers
  monitorComputersReceieveLoop(MainServerNode).

monitorComputersReceieveLoop(MainServerNode)->
  receive
    {nodedown,Node} ->moveSimulator:castPlease({nodedown,papapapokerface}), gen_server:cast({global,MainServerNode},{nodedown, Node});
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

%retunrs the sum of all  processes in my node + all other nodes
handle_call({getNumberOfProcesses}, _From, State = #mainServer_state{}) ->
  Processes = State#mainServer_state.processes,
  NumOfProcesses = lists:sum([Num||{_Node,Num}<-Processes]) + length(processes()),
  {reply, NumOfProcesses, State};


handle_call(_Request, _From, State = #mainServer_state{}) ->
  moveSimulator:castPlease({missedCallmainServer, request, _Request, from, _From}),
  {reply, ok, State}.



deleteallETS() -> First = ets:first(etsRobins),
  deleteallETS(First).
deleteallETS('$end_of_table') -> ok;
deleteallETS(Todelete) ->
  Next = ets:next(etsRobins,Todelete),
  ets:delete(etsRobins,Todelete),
  deleteallETS(Next).

%takes a list of pids from ETSX or ETSY, and updates their location in the ETSROBINS
updateEts(_,[],_,_)-> ok;
updateEts(OriginLocation,[Pid|Pids],XorY,Node)-> Location =OriginLocation ,
  IsMember = ets:member(etsRobins,{Pid,Node}),
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
handle_cast({etsUpdate,FromNode,EtsX,EtsY,NumOfProcesses}, State = #mainServer_state{}) ->
  spawn(fun()-> [updateEts(X,PidList,x,FromNode)||{X,PidList}<-EtsX],
                [updateEts(Y,PidList,y,FromNode)||{Y,PidList}<-EtsY] end),
  NewProcesses = newProcesses(FromNode,NumOfProcesses,State#mainServer_state.processes,[]),
  {noreply, State#mainServer_state{processes = NewProcesses}};

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
  io:format("ive got node down and i cannot lie: ~p~n",[{nodedown,node, MyNode}]),
  % update boarders on all remaining computerServers
  % delete from EtsRobins and spawn new ones, and send the locations to transfer to the new computer
  % if 4 nodes -> computer to the right or to the left takes it and gets X,Y locations
  % if 3 nodes and sx = 0 and ex = 0 then count the nodes
  % if 2 nodes -> takes all and get the locations
  ComputerNodes = State#mainServer_state.computerNodes,
  ComputerAreas = State#mainServer_state.computerAreas,
  ZipLists = lists:zip(ComputerNodes,ComputerAreas),
  [{MySx,MyEx,MySy,MyEy}]= [Area||{Node,Area}<-ZipLists, Node == MyNode], % take the fallen node areas
  Size = length(ComputerNodes), % before the "fall down"
  NewComputerNodes = ComputerNodes -- [MyNode],
  EtsRobinsList = ets:tab2list(etsRobins),
  %delete all the Dead Robins from EtsRobins:
   [ets:delete(etsRobins,{Pid,Node})||{{Pid,Node},_}<-EtsRobinsList, Node==MyNode],

  if ((Size == 4) or ((Size == 3) and ((MySx /= 0) or (MyEx /= 2000)))) -> % case: take computer to the right or the left
      [{ChosenNode, {CSx,CEx,CSy,CEy}}] = [{Node, {Sx, Ex, Sy, Ey}} ||{Node,{Sx,Ex,Sy,Ey}}<- ZipLists,(MySy == Sy),(MyEy == Ey),(Node /= MyNode)],
     moveSimulator:castPlease({chosenNode,{ChosenNode, {CSx,CEx,CSy,CEy}}}),
      ChosenNewArea= {0,2000,CSy,CEy},
      NewComputerAreas = newComputerAreas(ComputerAreas,{MySx,MyEx,MySy,MyEy},{CSx,CEx,CSy,CEy},ChosenNewArea),% removing the dead Node and update the chosen area
      [gen_server:cast({global,Node},{newBoarders,NewComputerNodes,NewComputerAreas})||Node<-NewComputerNodes], % send cast to change server boarders
      ListOfXY = [{X,Y}||{{_Pid,NodeRobin},{X,Y}}<-EtsRobinsList,NodeRobin == MyNode ], % saves all the {X,Y} locations of the dead node
      gen_server:cast({global,ChosenNode},{newRobinsAtXY,ListOfXY}),
    {noreply, State#mainServer_state{computerNodes = NewComputerNodes, computerAreas = NewComputerAreas}};

    (Size == 3) -> % special case: take the other 2 nodes and extend each of them and spawn new Robins
      NewComputerAreas = [{Sx,Ex,0,2000}|| {Sx,Ex,Sy,Ey}<-ComputerAreas, {Sx,Ex,Sy,Ey} /= {MySx,MyEx,MySy,MyEy}],
      [NodeL] = [Node||{Node,{Sx,_Ex,_Sy,_Ey}}<-lists:zip(NewComputerNodes,NewComputerAreas),Sx == 0],
      [NodeR] = [Node||{Node,{_Sx,Ex,_Sy,_Ey}}<-lists:zip(NewComputerNodes,NewComputerAreas),Ex == 2000],

    [gen_server:cast({global,Node},{newBoarders,NewComputerNodes,NewComputerAreas})||Node<-NewComputerNodes],

      ListL =  [{X,Y}||{{_Pid,NodeRobin},{X,Y}}<-EtsRobinsList,NodeRobin == MyNode, X<1001],
      ListR =  [{X,Y}||{{_Pid,NodeRobin},{X,Y}}<-EtsRobinsList,NodeRobin == MyNode, X>1000],
    gen_server:cast({global,NodeL},{newRobinsAtXY,ListL}), % tell the nodeL to spawn N new Robins
    gen_server:cast({global,NodeR},{newRobinsAtXY,ListR}),
      {noreply, State#mainServer_state{computerNodes = NewComputerNodes, computerAreas = NewComputerAreas}};

    (Size == 2) -> %case:  the left node takes all and get the locations
      [LonleyNode] = NewComputerNodes,
      gen_server:cast({global,LonleyNode},{newBoarders,NewComputerNodes,[{0,2000,0,2000}]}),
      ListOfXY = [{X,Y}||{{_Pid,NodeRobin},{X,Y}}<-EtsRobinsList,NodeRobin == MyNode ], % saves all the {X,Y} locations of the dead node
      gen_server:cast({global,LonleyNode},{newRobinsAtXY,ListOfXY}),
      {noreply, State#mainServer_state{computerNodes = NewComputerNodes, computerAreas = [{0,2000,0,2000}]}};


      true-> io:format("everyone i knew died, bye bye"),  %no more nodes -> terminate
           gen_server:stop({global,node()}),
          {noreply, State#mainServer_state{computerNodes = [], computerAreas = []}}
  end;

%user inserted new Stats :{Radius,NumofRobins,DemiZone,ORIGINATOR_INTERVAL,MaxVelocity,WindowSize,TTL}
%Restart ComputerServers with the new Stats, also send each ComputerServer his old robins XY values
handle_cast({newStats,NewSpecs}, State = #mainServer_state{}) ->
  deleteallETS(),
  Nodes = State#mainServer_state.computerNodes,
%%  [gen_server:stop({global,Node})||Node<-Nodes],
  [gen_server:cast({global, Node}, stop)||Node<-Nodes],

%%  [gen_server:cast({global,Node},{stopping})||Node<-Nodes],
  receive
    after 500 -> ok
  end,
  ComputerNodes = State#mainServer_state.computerNodes,
  ComputerAreas = State#mainServer_state.computerAreas,
  spawnComputer(ComputerNodes,ComputerAreas, NewSpecs,loop),

  moveSimulator:castPlease({newStats, NewSpecs }),

  {noreply, State};

handle_cast(_Request, State = #mainServer_state{}) ->
  moveSimulator:castPlease({missedCastMainSer, request, _Request}),

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

% removing the dead Node and update the chosen area
newComputerAreas(ComputerAreas,DeadArea, ChosenOldArea, ChosenNewArea) ->newComputerAreas(ComputerAreas,DeadArea, ChosenOldArea, ChosenNewArea,[]).
newComputerAreas([],_DeadArea, _ChosenOldArea, _ChosenNewArea,List) ->List;
newComputerAreas([DeadArea|T],DeadArea, ChosenOldArea, ChosenNewArea,List) ->newComputerAreas(T,DeadArea, ChosenOldArea, ChosenNewArea,List);
newComputerAreas([ChosenOldArea|T],DeadArea, ChosenOldArea, ChosenNewArea,List) ->newComputerAreas(T,DeadArea, ChosenOldArea, ChosenNewArea, List ++ [ChosenNewArea]);
newComputerAreas([H|T],DeadArea, ChosenOldArea, ChosenNewArea,List) ->newComputerAreas(T,DeadArea, ChosenOldArea, ChosenNewArea,List ++ [H]).

%returns a list with the updated Processes
newProcesses(FromNode, NumOfProcesses, [{FromNode,_OldProcesses}|T], List)-> List ++ [{FromNode, NumOfProcesses}] ++ T; % update node
newProcesses(FromNode, NumOfProcesses, [H|T], List)-> newProcesses(FromNode, NumOfProcesses, T, List ++ [H]); % keep searching in the list
newProcesses(FromNode, NumOfProcesses, ProcessesList, List)-> L = List ++ ProcessesList, L ++ [{FromNode, NumOfProcesses}]. %case a new Node


refreshtimer(Gui)->
  receive
  after  1000 div ?RefreshRate -> gen_statem:cast(Gui, {refresh, ets:tab2list(etsRobins)})
  end,
  refreshtimer(Gui).