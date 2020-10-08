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
  code_change/3,castPlease/1]).

-define(SERVER, ?MODULE).
-define(N, 40). % number of processes in all the program "Robins"
-define(DemilitarizedZone, 50). % how much area to add to each computer, "Demilitarized zone".
-define(updateMainEts, 20). % refresh rate to mainServer EtsRobins


-record(computerStateM_state, {computerNodes,computersArea, myArea,myMonitor,mainServer}).
%%test TODO delete
castPlease(MSG)-> gen_server:cast({global, tal@ubuntu},{test,MSG}).
%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(List::list()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link([ComputerNodes,ComputersArea,MainServerNode]) ->

  {ok, Pid} =  gen_server:start_link({global, node()}, ?MODULE, [ComputerNodes,ComputersArea,MainServerNode],[]),
%% gen_server:start_link({global, node()}, ?MODULE, [ComputerNodes,ComputersArea,MainServerNode],[]),
  Mymonitor = spawn_link(fun()->monitorRobins(Pid) end ),%spawns a process to monitor all the move simulators.
receive after 500-> ok end,

  gen_server:call(Pid,{updateMyMonitor,Mymonitor}),
  monitorAllRobins(Mymonitor),

  spawn_link(fun()->updateMainServerEts(MainServerNode) end).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #computerStateM_state{}} | {ok, State :: #computerStateM_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([ComputerNodes,ComputersArea,MainServerNode]) -> %gets the area of the computer, the borders.
  %the ETS is build like this: [{Location1,[pid1,pid2...]},{Location2,[pid1,pid2...]},....]
  ets:new(etsX,[ordered_set,public,named_table,{read_concurrency, true},{write_concurrency, true}]),
  ets:new(etsY,[ordered_set,public,named_table,{read_concurrency, true},{write_concurrency, true}]),
  MyArea = getArea(ComputerNodes,ComputersArea,node()),
  initRobins(MyArea),   %% spawn N/4 Robins and monitors them

  {ok, #computerStateM_state{computerNodes = ComputerNodes, computersArea = ComputersArea, myArea = MyArea,myMonitor = updatedLater ,mainServer = MainServerNode}}. %saves the area border

% gets My Area from lists of nodes and areas
getArea([],[],_)-> areaCantBeFound;
getArea([MyNode|_],[Area|_],MyNode) -> Area;
getArea([_|T1],[_|T2],MyNode) -> getArea(T1,T2,MyNode).

initRobins(MyArea) -> %spawn N/4 Robins
  Loop = lists:seq(1,?N div 4),
  %spawn a Robin and monitor it, we add the DemilitarizedZone, so the moveSimulator will know it
  [spawn(moveSimulator,start_link,[[MyArea,?DemilitarizedZone,self()]])|| _<- Loop].


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
%%===================================================================================
%sendMsg = the Msg needs to be sent to neighbor computerServer

handle_call({sendMsg,To, {FromNeighborPid,FromNeighborNode},Msg,MoveSimFrom}, _From, State = #computerStateM_state{}) ->
  try %if the call fails
    Reply = gen_server:call({global, FromNeighborNode},{receiveMsg,To, {FromNeighborPid,FromNeighborNode},Msg,MoveSimFrom}),
    {reply, Reply, State}
  catch
    _:_-> {reply, notSent, State}
  end;

handle_call({updateMyMonitor,Mymonitor}, _From, State = #computerStateM_state{}) ->
{reply, updatedMyMonitor, State#computerStateM_state{myMonitor = Mymonitor}};

%receiveMsg = recieved the msg from neighbor Computer and send it to the right pid
handle_call({receiveMsg,To, {FromNeighborPid,_FromNeighborNode},Msg,MoveSimFrom}, _From, State = #computerStateM_state{}) ->
  try %if the call fails
    Reply = gen_server:call(FromNeighborPid,{receiveMsg,To,Msg,MoveSimFrom}),
    {reply, Reply, State}
  catch
    _:_-> {reply, notSent, State}
  end;
%%===================================================================================

%       true -> {NewStartX,NewEndX,NewStartY,NewEndY,ToTerminate} = gen_server:call(PCPid,{updateBorders,X,Y})
%Move simulator wants to know the new borders, if he is out of the area, he should terminate and a new move simulator should be created in the computer
handle_call({updateBorders, {X,Y,Dir,Vel}}, _From, State = #computerStateM_state{}) ->
  {StartX,EndX,StartY,EndY} = State#computerStateM_state.myArea,

  if ((Y<StartY) or (Y > EndY) or (X < StartX) or (X > EndX)) ->%check if the movesim borders needs to be updated
      CompTrgt = getComputer(X,Y,State#computerStateM_state.computersArea,State#computerStateM_state.computerNodes),
%%    castPlease({computerTarger,CompTrgt,mynode,node()}),
      ToTerminate = true,
      try
        gen_server:cast({global,CompTrgt},{createBatman,{round(X),round(Y),Dir,Vel}})
      catch _:Err->     castPlease({borderserror,Err})
        ,connectionError
      end;
    true -> ToTerminate = false %New borders
  end,
  {reply, {StartX,EndX,StartY,EndY,ToTerminate}, State};

%returns {PidTo,NodeTo} from batmans known(PidFrom)
handle_call({getKnownFrom, PidFrom}, _From, State = #computerStateM_state{}) ->
  {PidTo,NodeTo} = gen_server:call(PidFrom,{getKnownFrom}),
{reply, {PidTo,NodeTo}, State};

handle_call(Request, _From, State = #computerStateM_state{}) ->
  castPlease({computerServerMissedCalls, Request}),
  {reply, computerServerMissedCalls, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #computerStateM_state{}) ->
  {noreply, NewState :: #computerStateM_state{}} |
  {noreply, NewState :: #computerStateM_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #computerStateM_state{}}).

%%===================================================================================
handle_cast({createBatman,{X,Y,Dir,Vel}}, State = #computerStateM_state{})->
%spawn a Robin and monitor it, we add the DemilitarizedZone, so the moveSimulator will know it
  spawn(moveSimulator,start_link,[[State#computerStateM_state.myArea,?DemilitarizedZone,self(),{X,Y,Dir,Vel}]]),
{noreply, State};

handle_cast({sendOGMtoNeighborsX,MyX,MyY,OGM,{PidFrom,NodeFrom}}, State = #computerStateM_state{}) -> %todo todo only temp
  try
  {StartX,EndX,_,_}= State#computerStateM_state.myArea,
  DisToLeft = MyX - StartX,
  DisToRight = EndX - MyX,
  if (DisToRight < DisToLeft) ->% im close to the right border
    if (EndX < 2000) -> % there is a computer to te right
      [Node] = neighbor(State,right),
      gen_server:cast({global, Node},{ogmFromNeighbor,MyX,MyY,OGM,{PidFrom,NodeFrom}});
      true->{noreply, State}
    end;
  true -> % else DisToRight >= DisToLeft
    if (StartX > 0) -> % there is a computer to the left
      [Node2] = neighbor(State,left),
      gen_server:cast({global, Node2},{ogmFromNeighbor,MyX,MyY,OGM,{PidFrom,NodeFrom}});
      true->{noreply, State}
    end
  end
    catch _:_ -> noNeighbor %no Neighbor probebly node down
end,
  {noreply, State};
handle_cast({sendOGMtoNeighborsY,MyX,MyY,OGM,{PidFrom,NodeFrom}}, State = #computerStateM_state{}) -> %todo todo only temp
  try
  {_,_,StartY,EndY}= State#computerStateM_state.myArea,
  DisToUp = MyY - StartY,
  DisToDown = EndY - MyY,
  if (DisToDown < DisToUp) ->% im close to the right border
    if (EndY < 2000) -> % there is a computer to down
      [Node] = neighbor(State,down),

      gen_server:cast({global, Node},{ogmFromNeighbor,MyX,MyY,OGM,{PidFrom,NodeFrom}});
    true->{noreply, State}
    end;
    true -> % else DisToRight >= DisToLeft
      if (StartY > 0) -> % there is a computer to the left
        [Node2] = neighbor(State,up),

        gen_server:cast({global, Node2},{ogmFromNeighbor,MyX,MyY,OGM,{PidFrom,NodeFrom}});
        true->{noreply, State}
      end
  end
catch _:_ -> noNeighbor %no Neighbor probebly node down
end,
  {noreply, State};
%%===================================================================================

%%===================================================================================
handle_cast({ogmFromNeighbor,MyX,MyY,OGM,{PidFrom,NodeFrom}}, State = #computerStateM_state{}) ->
%%  send the OGM to all the Robins in the radius in my computer
  ListOfRobins = moveSimulator:robinsInRadiusForRemote(MyX,MyY),
%%  castPlease({ogmFromNeigh,mynode,node(),pidnode, {PidFrom, NodeFrom},OGM,listOFROBINS,ListOfRobins}),
  [gen_server:cast(PidTo,{ogm,OGM,{PidFrom,NodeFrom}}) ||{PidTo,_NodeTo} <- ListOfRobins],
%%  [castPlease({PidTo1, {ogm, OGM, {PidFrom, NodeFrom}},mynode,node()}) ||{PidTo1,_NodeTo1} <- ListOfRobins],
  {noreply, State};

%%===================================================================================
%Sends a addRobin message to myMonitor to add to monitored processes
handle_cast({monitorMe,From}, State = #computerStateM_state{}) ->
  State#computerStateM_state.myMonitor !   {addRobin, From},
  {noreply, State};

%removes Robin from ETS and cast mainServer to do so(Same as above but without X,Y
handle_cast({removeRobin,Pid}, State = #computerStateM_state{}) ->
%%  {X,Y} = searchXYbyPid(Pid),
  MainServerNode = State#computerStateM_state.mainServer,
  gen_server:cast({global, MainServerNode},{removeRobin,Pid,node()}),
%%  castPlease({removing,pid,Pid,xy,X,Y,ets:tab2list(etsX),ets:tab2list(etsY)}),
%%  removeRobin(Pid,X,Y, State),
  {noreply, State};

handle_cast({msgSent,From,To}, State = #computerStateM_state{}) ->
  castPlease({sending,from,From,to,To}),
  gen_server:cast({global, State#computerStateM_state.mainServer},{addMessage, From,To}),
{noreply, State};


%Robin crashed so we generate a new one
handle_cast({generateRobin}, State = #computerStateM_state{}) ->
  MyArea = State#computerStateM_state.myArea,
  spawn(moveSimulator,start_link,[[MyArea,?DemilitarizedZone,self()]]),
  {noreply, State};

%someone died, update Boarders, if my boarder updated change it too
handle_cast({newBoarders,NewComputerNodes,NewComputerAreas}, State = #computerStateM_state{}) ->
  [NewArea] = [Area||{Node,Area}<-lists:zip(NewComputerNodes,NewComputerAreas),Node == node()], % takes my area from the list of new areas
  {noreply, State#computerStateM_state{computerNodes = NewComputerNodes,computersArea = NewComputerAreas,myArea = NewArea}};

%(someone died) spawn new Robins at each {X,Y} from ListOfXY
handle_cast({newRobinsAtXY,ListOfXY}, State = #computerStateM_state{}) ->
  MyArea = State#computerStateM_state.myArea,
  castPlease({newRobinsAtXY,node,node(),area,MyArea}),
  [spawn(moveSimulator,start_link,[[MyArea,?DemilitarizedZone,self(),{X,Y,0,0}]])|| {X,Y}<- ListOfXY],
  {noreply, State#computerStateM_state{}};

%%===================================================================================
handle_cast(Request, State = #computerStateM_state{}) ->
  castPlease({missedMessegCOMPs,request,Request}),

  {noreply, State}.
%%===================================================================================
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
%%Neighbor -> returns the node of the computer to my Direction
neighbor(State,Dir) ->
  ZipLists = lists:zip(State#computerStateM_state.computerNodes,State#computerStateM_state.computersArea),
  {MySx,_MyEx,MySy,_MyEy} = State#computerStateM_state.myArea,
  case Dir of
    right -> [Node ||{Node,{_Sx,Ex,Sy,_Ey}}<- ZipLists, Ex == 2000, MySy==Sy ];
    left -> [Node ||{Node,{Sx,_Ex,Sy,_Ey}}<- ZipLists, Sx == 0, MySy==Sy];
    up -> [Node ||{Node,{Sx,_Ex,Sy,_Ey}}<- ZipLists, Sy == 0 , MySx==Sx];
    down -> [Node ||{Node,{Sx,_Ex,_Sy,Ey}}<- ZipLists, Ey == 2000,MySx==Sx];
  true->   castPlease({list,ZipLists,dir,Dir,node,error})

end.


%getComputer returns the target computer Node to transfer the movesimulator process to,
% ComputerNodes-> [tal@ubuntu,yossi@megatron....], size 4
% ComputersArea-> [{startX,endX,startY,endY},...] size 4
% getComputer(X,Y,computersArea,computerNodes),

getComputer(_,_,[],[])-> nodeNotFound;
getComputer(X,Y,[{StartX,EndX,StartY,EndY}|_],[Node|_]) when ((StartX<X) and (X<EndX) and (StartY<Y) and (Y<EndY)) ->Node;
getComputer(X,Y,[_|Areas],[_Node|Nodes]) -> getComputer(X,Y,Areas,Nodes).

%removes Robin from ETS and cast mainServer to do so
removeRobin(Pid,X,Y, State = #computerStateM_state{}) ->
%%  [ {_,TempX}] = ets:lookup(etsX,X),
%%  [{_, TempY}]= ets:lookup(etsY,Y),
%%  ListX = TempX -- [Pid],% remove the pid from the old location
%%  ListY = TempY -- [Pid],
%%  if length(ListX)>0 ->ets:insert(etsX,[{X,ListX}]);
%%    true->  ets:delete(etsX,X)
%%  end,
%%  if length(ListY)>0 ->ets:insert(etsY,[{Y,ListY}]);
%%    true->  ets:delete(etsY,Y)
%%  end,
%%  castPlease({removeingRobin,Pid}),
  %cast to main server to remove the pid from the main etsRobins
  MainServerNode = State#computerStateM_state.mainServer,
  gen_server:cast({global, MainServerNode},{removeRobin,Pid,node()}). % todo change





searchXYbyPid(Pid) ->
  FirstX = ets:first(etsX),
  FirstY = ets:first(etsY),
  X = search(etsX,FirstX,Pid),
  Y = search(etsY,FirstY,Pid),
  {X,Y}.

%Look for The right X or Y for an existing Pid int he ets given
search(_,'$end_of_table',_) -> notfound;
search(Ets, Key,Pid) ->
  [{_Key, Pids}] = ets:lookup(Ets,Key),
  IsMember =  lists:member(Pid,Pids), %check if the pid is in the list of pids
  if
    IsMember-> Key;
    true ->
        search(Ets,ets:next(Ets,Key),Pid)
  end.

% sending monitorMe cast to myComputerServer for every pid in the ets
monitorAllRobins(MyMonitor) ->
  [([MyMonitor ! {addRobin,Pid} ||Pid<-Pids])||{_Key,Pids}<-ets:tab2list(etsX)].


updateMainServerEts(MainServerNode)-> receive
                                      after 1000 div ?updateMainEts -> gen_server:cast({global, MainServerNode},{etsUpdate,node(),ets:tab2list(etsX),ets:tab2list(etsY),length(processes())})
                                      end, updateMainServerEts(MainServerNode).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%monitors all robins(move simulators)
monitorRobins(MyComputerServer) ->
  receive
    {addRobin, Pid} ->
      erlang:monitor(process,Pid);

    {'DOWN', Ref, process, Pid, normal} ->
      gen_server:cast(MyComputerServer,{removeRobin, Pid});

    {'DOWN', Ref, process, Pid,  Reason} ->
      castPlease({notnormal,ref,Ref,pid,Pid,reason,Reason}),
      gen_server:cast(MyComputerServer,{removeRobin, Pid}),
      gen_server:cast(MyComputerServer,{generateRobin});

    SomeError ->   castPlease({zeze,SomeError})
  end,
  monitorRobins(MyComputerServer).