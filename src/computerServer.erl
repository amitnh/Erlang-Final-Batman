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
-define(N, 12). % number of processes in all the program "Robins"
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
  Mymonitor = spawn_link(fun()->monitorRobins() end ),
%%  gen_server:start_link({global, node()}, ?MODULE, [ComputerNodes,ComputersArea],[{debug,[trace]}]),
  gen_server:start_link({global, node()}, ?MODULE, [ComputerNodes,ComputersArea,Mymonitor,MainServerNode],[]),
  receive
    after 500-> ok
  end,
  %%  spawn_link(fun()->testMsgSending() end),
  spawn_link(fun()->updateMainServerEts(MainServerNode) end).

updateMainServerEts(MainServerNode)-> receive
                          after 1000 div ?updateMainEts -> gen_server:cast({global, MainServerNode},{etsUpdate,node(),ets:tab2list(etsX),ets:tab2list(etsY)})
                        end, updateMainServerEts(MainServerNode).

testMsgSending()-> receive
                        after 5000  -> First = ets:first(etsX),
                       From = takeNelement(First,rand:uniform(?N div 8)),
                       To = takeNelement(First,rand:uniform(?N div 8)),
                        castPlease({sendingMsg,from,From,to,To}),
                        gen_server:cast(From,{sendMsg,{To,node()},{first,msg},helloBanana})
                        end, testMsgSending().

takeNelement(X, 0) ->
  try
  [{_Key,[Pid|_]}]=ets:lookup(etsX,X), Pid
  catch
    _-> castPlease({errorintakeNelement,x,X}), ets:first(etsX)
  end;
takeNelement(X, N) ->
  takeNelement(ets:next(etsX,X), N-1).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #computerStateM_state{}} | {ok, State :: #computerStateM_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([ComputerNodes,ComputersArea,Mymonitor,MainServerNode]) -> %gets the area of the computer, the borders.
  %the ETS is build like this: [{Location1,[pid1,pid2...]},{Location2,[pid1,pid2...]},....]
  ets:new(etsX,[ordered_set,public,named_table,{read_concurrency, true},{write_concurrency, true}]),
  ets:new(etsY,[ordered_set,public,named_table,{read_concurrency, true},{write_concurrency, true}]),
  MyArea = getArea(ComputerNodes,ComputersArea,node()),
  initRobins(MyArea),   %% spawn N/4 Robins and monitors them
  {ok, #computerStateM_state{computerNodes = ComputerNodes, computersArea = ComputersArea, myArea = MyArea,myMonitor = Mymonitor,mainServer = MainServerNode}}. %saves the area border

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
handle_call({sendMsg,To, {FromNeighborPid,FromNeighborNode},Msg}, _From, State = #computerStateM_state{}) ->
  try %if the call fails
    Reply = gen_server:call({global, FromNeighborNode},{receiveMsg,To, {FromNeighborPid,FromNeighborNode},Msg}),
    {reply, Reply, State}
  catch
    true-> {reply, notSent, State}
  end;

%receiveMsg = recieved the msg from neighbor Computer and send it to the right pid
handle_call({receiveMsg,To, {FromNeighborPid,_FromNeighborNode},Msg}, _From, State = #computerStateM_state{}) ->
  try %if the call fails
    Reply = gen_server:call(FromNeighborPid,{receiveMsg,To,Msg}),
    {reply, Reply, State}
  catch
    true-> {reply, notSent, State}
  end;
%%===================================================================================

%       true -> {NewStartX,NewEndX,NewStartY,NewEndY,ToTerminate} = gen_server:call(PCPid,{updateBorders,X,Y})
%Move simulator wants to know the new borders, if he is out of the area, he should terminate and a new move simulator should be created in the computer
handle_call({updateBorders, {X,Y,Dir,Vel}}, _From, State = #computerStateM_state{}) ->
  {StartX,EndX,StartY,EndY} = State#computerStateM_state.myArea,

  if ((Y<StartY) or (Y > EndY) or (X < StartX) or (X > EndX)) ->
      CompTrgt = getComputer(X,Y,State#computerStateM_state.computersArea,State#computerStateM_state.computerNodes),
      ToTerminate = true,
      try
        gen_server:cast({global,CompTrgt},{createBatman,{X,Y,Dir,Vel}})
      catch _-> connectionError
      end;
    true -> ToTerminate = false %New borders
    end,
  {reply, {StartX,EndX,StartY,EndY,ToTerminate}, State};

handle_call(_Request, _From, State = #computerStateM_state{}) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #computerStateM_state{}) ->
  {noreply, NewState :: #computerStateM_state{}} |
  {noreply, NewState :: #computerStateM_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #computerStateM_state{}}).

%%===================================================================================
handle_cast({createBatman,RobinState}, State = #computerStateM_state{})->
%spawn a Robin and monitor it, we add the DemilitarizedZone, so the moveSimulator will know it
  spawn(moveSimulator,start_link,[[State#computerStateM_state.myArea,?DemilitarizedZone,RobinState]]),
{noreply, State};

handle_cast({sendOGMtoNeighborsX,MyX,MyY,OGM,{Pid,Node}}, State = #computerStateM_state{}) -> %todo todo only temp
  {StartX,EndX,_,_}= State#computerStateM_state.myArea,
  DisToLeft = MyX - StartX,
  DisToRight = EndX - MyY,
  if DisToRight < DisToLeft ->% im close to the right border
    if EndX < 2000 -> % there is a computer to te right
      [Node] = neighbor(State,right),
      gen_server:cast({global, Node},{ogmFromNeighbor,MyX,MyY,OGM,{Pid,Node}})
    end;
  true -> % else DisToRight >= DisToLeft
    if StartX > 0 -> % there is a computer to the left
      [Node] = neighbor(State,left),
      gen_server:cast({global, Node},{ogmFromNeighbor,MyX,MyY,OGM,{Pid,Node}})
    end
  end,
  {noreply, State};
handle_cast({sendOGMtoNeighborsY,MyX,MyY,OGM,{Pid,Node}}, State = #computerStateM_state{}) -> %todo todo only temp
  {_,_,StartY,EndY}= State#computerStateM_state.myArea,
  DisToUp = MyY - StartY,
  DisToDown = EndY - MyY,
  if DisToDown < DisToUp ->% im close to the right border
    if EndY < 2000 -> % there is a computer to te right
      [Node] = neighbor(State,down),
      gen_server:cast({global, Node},{ogmFromNeighbor,MyX,MyY,OGM,{Pid,Node}})
    end;
    true -> % else DisToRight >= DisToLeft
      if StartY > 0 -> % there is a computer to the left
        [Node] = neighbor(State,up),
        gen_server:cast({global, Node},{ogmFromNeighbor,MyX,MyY,OGM,{Pid,Node}})
      end
  end,
  {noreply, State};
%%===================================================================================

%%===================================================================================
handle_cast({Node,{ogmFromNeighbor,MyX,MyY,OGM,{Pid,Node}}}, State = #computerStateM_state{}) ->
%%  send the OGM to all the Robins in the radius in my computer
  ListOfRobins = moveSimulator:robinsInRadiusForRemote(MyX,MyY),
  [gen_server:cast(Pid,{ogm,OGM,{Pid,Node}}) ||{Pid,_Node} <- ListOfRobins],
  {noreply, State};

%%===================================================================================
%Sends a addRobin message to myMonitor to add to monitored processes
handle_cast({monitorMe,From}, State = #computerStateM_state{}) ->
  State#computerStateM_state.myMonitor !   {addRobin, From},
  {noreply, State};

%removes Robin from ETS and cast mainServer to do so
handle_cast({removeRobin,Pid,X,Y}, State = #computerStateM_state{}) ->
  removeRobin(Pid,X,Y, State),
  {noreply, State};



%TODO TODO
%removes Robin from ETS and cast mainServer to do so(Same as above but without X,Y
handle_cast({removeRobin,Pid}, State = #computerStateM_state{}) ->
  {X,Y} = searchXYbyPid(Pid),
  removeRobin(Pid,X,Y, State),
  {noreply, State};


%%===================================================================================
handle_cast(_Request, State = #computerStateM_state{}) ->
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
  case Dir of
    right -> [Node ||{Node,{_Sx,Ex,_Sy,_Ey}}<- ZipLists, Ex = 2000];
    left -> [Node ||{Node,{Sx,_Ex,_Sy,_Ey}}<- ZipLists, Sx = 0];
    up -> [Node ||{Node,{_Sx,_Ex,_Sy,Ey}}<- ZipLists, Ey = 2000];
    true -> [Node ||{Node,{_Sx,_Ex,Sy,_Ey}}<- ZipLists, Sy = 0] % case down
  end.


%getComputer returns the target computer Node to transfer the movesimulator process to,
% ComputerNodes-> [tal@ubuntu,yossi@megatron....], size 4
% ComputersArea-> [{startX,endX,startY,endY},...] size 4
% getComputer(X,Y,State#computerStateM_state.computersArea,State#computerStateM_state.computerNodes),

getComputer(_,_,[],[])-> nodeNotFound;
getComputer(X,Y,[{StartX,EndX,StartY,EndY}|_],[Node|_]) when ((StartX<X) and (X<EndX) and (StartY<Y) and (Y<EndY)) ->Node;
getComputer(X,Y,[_|Areas],[_Node|Nodes]) -> getComputer(X,Y,Areas,Nodes).

%removes Robin from ETS and cast mainServer to do so
removeRobin(Pid,X,Y, State = #computerStateM_state{}) ->   {_,TempX} = ets:lookup(etsX,X),
  {_,TempY}= ets:lookup(etsY,Y),
  ListX = TempX -- [Pid],% remove the pid from the old location
  ListY = TempY -- [Pid],
  if length(ListX)>0 ->ets:insert(etsX,[{X,ListX}]);
    true->  ets:delete(etsX,X)
  end,
  if length(ListY)>0 ->ets:insert(etsY,[{Y,ListY}]);
    true->  ets:delete(etsY,Y)
  end,
  gen_server:cast({global, State#computerStateM_state.mainServer},{removeRobin,Pid}).


monitorRobins(MyComputerServer) ->

  receive
    {addRobin, Pid} ->   erlang:monitor(process,Pid);
    {'DOWN', Ref, process, Pid, {normal,X,Y}} ->
      castPlease({ref,Ref,pid,Pid}),
      gen_server:cast(MyComputerServer,{removeRobin, Pid,X,Y});
    {'DOWN', Ref, process, Pid,  Reason} ->
      gen_server:cast(MyComputerServer,{generate, Pid});
    _ ->   ok
  end,
monitorRobins(MyComputerServer).

searchXYbyPid(Pid) ->
  FirstX = ets:first(etsX),
  FirstY = ets:first(etsY),
  X = search(etsX,FirstX,Pid),
  Y = search(etsY,FirstY,Pid),
  {X,Y}.

%Look for The right X or Y for an existing Pid int he ets given
search(_,'$end_of_table',_) -> notfound;
search(Ets, Key,Pid) ->
  {_Key,Pids} = ets:lookup(Key),
  IsMember =  lists:member(Pid,Pids), %check if the pid is in the list of pids
  if
    IsMember-> Key;
    true -> search(Ets,ets:next(Ets,Key),Pid)
  end.
