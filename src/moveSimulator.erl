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
-define(updateEts ,20). %how many time per second to update the ETS's
-define(velMax , 100). %range of the random velocity of the node in meter/milisec
-define(timeRange ,{1000,5000}). %range of the random time to change direction of the node in milisec
-define(radius ,100).

-record(moveSimulator_state, {startX,endX,startY,endY,demiZone,myX,myY,time,velocity,direction}).


%%test TODO delete
castPlease(MSG)-> gen_server:cast({global, tal@ubuntu},{test,MSG}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(List::term()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link([Area,DemiZone]) ->
  {ok,Pid} = gen_server:start_link( ?MODULE, [Area,DemiZone], [{debug,[trace]}]), %TODO change the name ?MODULE, it wont work with more then 1 computer
%%  castPlease(moveSimulatorOnline),
  spawn_link(fun()->etsTimer(Pid) end),
  spawn_link(fun()->vectorTimer(Pid) end).

%send a cast to update the main ets's every ?updateEts milisecounds
etsTimer(Pid)->TimeToWait = 1000 div ?updateEts, %time to wait for sending  ?updateEts msgs in 1 sec
            receive
              after TimeToWait -> gen_server:cast(Pid,{updateEts})
            end,
            etsTimer(Pid).
%update the random velocity,direction and updatetime to update
vectorTimer(Pid)->
  %this 3 are going to the vector (in the record):
  CurrTime = erlang:system_time(millisecond),
  Velocity = rand:uniform(?velMax*1000)/1000,
  Direction = rand:uniform(360), % in degrees

  {Min,Max} = ?timeRange,
  TimeToWait = Min + rand:uniform(Max-Min),
  gen_server:cast(Pid,{updateMovementVector,CurrTime,Velocity,Direction}),
  receive
    after TimeToWait -> ok
  end, vectorTimer(Pid).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #moveSimulator_state{}} | {ok, State :: #moveSimulator_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([{StartX,EndX,StartY,EndY},DemiZone]) ->
  batmanProtocol:start_link(self()), %creates batmanProtocol and link it to this process
  {X,Y} = startLocation(StartX,EndX,StartY,EndY), % put my new random location in the etsX and etsY
  {ok, #moveSimulator_state{startX = StartX,endX = EndX,startY = StartY,endY = EndY,demiZone = DemiZone,myX = X,myY = Y,time = erlang:system_time(millisecond),velocity=0,direction=0}};
init(_)-> castPlease(errorInArea).

%%---------------------------------------------------------------------------------------------------
%startLocation, insert a new location in the etsX and etsY maps
startLocation(StartX,EndX,StartY,EndY)->
  LocationX = StartX + rand:uniform(EndX-StartX), %returns a random integer in the computer Area
  LocationY = StartY + rand:uniform(EndY-StartY),
  ListX = listToUpdate(ets:lookup(etsX,LocationX),LocationX),
  ListY = listToUpdate(ets:lookup(etsY,LocationY),LocationY),
  ets:insert(etsX,ListX),
  ets:insert(etsY,ListY),
  {LocationX,LocationY}. %return {X,Y} value

%checks if the im the first one on that list in the ETS or not.
%the ETS is build like this: [{Location1,[pid1,pid2...]},{Location2,[pid1,pid2...]},....]
listToUpdate([],Location)-> [{Location,[self()]}];
listToUpdate([{Location,List}],_Location)->[{Location,List ++ [self()]}].
%%---------------------------------------------------------------------------------------------------
%% calculate the new locations of X and Y based on the random vector
%%---------------------------------------------------------------------------------------------------
updatedXYlocations(State)->
  X0 = State#moveSimulator_state.myX,
  Y0 = State#moveSimulator_state.myY,
  Vel = State#moveSimulator_state.velocity,
  Dir = State#moveSimulator_state.direction,
  CurrTime = erlang:system_time(millisecond),
  DeltaTime = CurrTime - State#moveSimulator_state.time,

  X = X0 + math:cos(Dir * math:pi() / 180)*Vel*DeltaTime/1000, % x = vt , trigo
  Y = Y0 + math:sin(Dir * math:pi() / 180)*Vel*DeltaTime/1000,

  {X,Y,CurrTime}.
%%---------------------------------------------------------------------------------------------------


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
%%handle_cast({updateArea,NewArea}, _) -> todo later
%%  {noreply, #moveSimulator_state{myArea = NewArea}};

handle_cast({updateMovementVector,CurrTime,Velocity,Direction}, State = #moveSimulator_state{}) ->
%%  castPlease(updateMovementVector),
  {noreply, State#moveSimulator_state{time = CurrTime,velocity = Velocity, direction = Direction}};

%updateEts updates the location of my PID in the etsX and etsY
handle_cast({updateEts}, State = #moveSimulator_state{}) ->
  {X,Y,CurrTime} = updatedXYlocations(State),
  RoundedOldX = round(State#moveSimulator_state.myX), % X in ets is rounded. not in myX record
  RoundedOldY = round(State#moveSimulator_state.myY),
  Tempx = ets:lookup(etsX,RoundedOldX),
  Tempy= ets:lookup(etsY,RoundedOldY),
  if length(Tempx)>0 -> [{_,ListX}]= Tempx; %check if lookup is empty
      true-> ListX =[]
   end,
  if length(Tempy)>0 -> [{_,ListY}]= Tempy;
    true-> ListY =[]
  end,

  OldListX = ListX -- [self()],% remove the pid from the old location
  OldListY = ListY -- [self()],
  if (length(OldListX) > 0) ->  ets:insert(etsX,[{RoundedOldX,OldListX}]);%put back the old Locations lists if not empty
    true-> ets:delete(etsX,RoundedOldX) % delete if empty
  end, %
  if (length(OldListY) > 0) ->  ets:insert(etsY,[{RoundedOldY,OldListY}]);%put back the old Locations lists if not empty
    true-> ets:delete(etsY,RoundedOldY) % delete if empty
  end,

  RoundedNewX = round(X),
  RoundedNewY = round(Y),
  [{_,NewListX}] = listToUpdate(ets:lookup(etsX,RoundedNewX),RoundedNewX),
  [{_,NewListY}] = listToUpdate(ets:lookup(etsY,RoundedNewY),RoundedNewY),
  ets:insert(etsX,[{RoundedNewX,NewListX}]), %insert the new Locations lists
  ets:insert(etsY,[{RoundedNewY,NewListY}]),
  {noreply, State#moveSimulator_state{myX = X,myY = Y,time = CurrTime}}; % todo check if it works


handle_cast({sendToNeighbors,Msg}, State = #moveSimulator_state{}) -> % Msg usually is OGM
  ListOfRobins = robinsInRadius(State),
  [gen_server:cast(RobinAdd,{ogm,Msg,{self(),node()}}) ||RobinAdd <- ListOfRobins], % sends the OGM and the sender address to all the neighbors
  {noreply, State};
handle_cast({sendMsg,Msg,{Pid,Node}}, State = #moveSimulator_state{}) ->
  {noreply, State};
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


%%===========================================================
%%robinsInRadius -> return all the {pid,node} of all the pids that in my radius
%%===========================================================
robinsInRadius(State) ->
  MyX = State#moveSimulator_state.myX,
  MyY = State#moveSimulator_state.myY,
  EndX =  State#moveSimulator_state.endX,
  EndY =  State#moveSimulator_state.endY,
  StartX =  State#moveSimulator_state.startX,
  StartY =  State#moveSimulator_state.startY,
  DemiZone = State#moveSimulator_state.demiZone,
  %Xlist and Ylist are all the pids in square of radius x radius
  Xlist = getPrev(etsX,MyX,MyX,[]) ++ getNext(etsX,MyX,MyX,[]),
  Ylist = getPrev(etsY,MyY,MyY,[]) ++ getNext(etsY,MyY,MyY,[]),

  % case im close to the border, i will send a request to computerServer to look for neighbors in other computers
  if (MyX + ?radius > EndX - DemiZone) or (MyX - ?radius > StartX + DemiZone) ->
    XlistRemoteCom = gen_server:call({global, tal@ubuntu},{getNeighborsX,MyX}) %todo: return a list of {Pid,node()}
    end,
  if (MyY + ?radius > EndY - DemiZone) or (MyY - ?radius > StartY + DemiZone) ->
    YlistRemoteCom = gen_server:call({global, tal@ubuntu},{getNeighborsY,MyY}) %todo: return a list of {Pid,node()}
    end,
  getPidsInCircle(MyX,MyY,Xlist++XlistRemoteCom,Ylist++YlistRemoteCom).




%%===========================================================
%% getPidsInCircle ->
%% input: my x,y location and 2 lists as follows:
%% Xlist = [{Xlocation,{Pid,node}},....] and Ylist and return only the pids that are in the radius
%%
%% output: is 1 list with all the Addresses in my radius -> [{Pid1,Node1},{Pid2,Node1},{Pid3,Node2},...]
%%===========================================================
getPidsInCircle(X,Y,Xlist,Ylist)-> Square = getSquare(Xlist,Ylist),
  Circle =getCircle(Square), % Square -> [{x,y,address},...]
  [Address||{_X,_Y,Address}<-Circle]. % returns only the Addresses back

%%getSquare returns {x,y,address},...] withing a square of radiusXradius
getSquare(Xlist,Ylist) -> getSquare(Xlist,Ylist,[]).
getSquare([],_,List)-> lists:filter(fun(X) -> X /= {} end, List); %removes all empty tuples from the list
%for each X we will lookup a similar Y withing the square
getSquare([{XHead,XPidHead}|XTail],Ylist,List) ->getSquare(XTail,Ylist,[getY(XHead,XPidHead,Ylist)|List]).

getY(_,_,[])-> {};
getY(XHead,XPidHead,[{YHead,XPidHead}|_])->{XHead,YHead,XPidHead};  %found a member in Ylist with the same address as X address
getY(XHead,XPidHead,[{_,_}|Ylist])->getY(XHead,XPidHead,Ylist).

getCircle(Square) -> lists:filter(fun({X,Y,_Address}) -> (X*X+Y*Y< ?radius*?radius) end, Square). % Square -> [{x,y,address},...]
%%===========================================================





%%===========================================================
%%% getNext and getPrev works for etsX or etsY
%% they return a list of Location and pids, in the same computerServer in the radius range
%%===========================================================
  getNext(_,_,'$end_of_table',List)-> List;
  getNext(_,MyX,NextX,List) when NextX - MyX > ?radius ->  List;  % case nextX not in range
  getNext(Ets,MyX,NextX,List)  -> % case nextX in range
    [{_Key,Pids}] = ets:lookup(Ets,NextX), % Value contains the list of Pids
    NewList = List ++ [{NextX,{Pid,node()}}||Pid<-Pids],
    getNext(Ets,MyX,ets:next(Ets,NextX),NewList).

getPrev(_,_,'$end_of_table',List)-> List;
getPrev(_,MyX,PrevX,List) when MyX - PrevX > ?radius ->  List;  % case prevX not in range
getPrev(Ets,MyX,PrevX,List)  -> % case prevX in range
  [{_Key,Pids}] = ets:lookup(Ets,PrevX), % Value contains the list of Pids
  NewList =  [{PrevX,{Pid,node()}}||Pid<-Pids] ++ List,
  getPrev(Ets,MyX,ets:next(Ets,PrevX),NewList).
%%===========================================================







% when X > MyX + ?radius

getRobins(Ets,MyX,MyX,DemiZone,List)  -> %first case just starting
  NextX = ets:next(Ets,MyX),
  PrevX = ets:prev(Ets,MyX),
  getRobins(Ets,MyX,PrevX,DemiZone,List) ++ getRobins(Ets,MyX,NextX,DemiZone,List);

getRobins(Ets,MyX,X,DemiZone,List) when X > MyX -> %case X > Myx
  if X > MyX + ?radius -> List; %out of the rectangle
    true-> NextX = ets:next(Ets,X),
      [{_Key,Value}] = ets:lookup(Ets,X), % todo, add NODE to the PID
    getRobins(Ets,MyX,NextX,DemiZone,Value ++ List)
  end;

getRobins(Ets,MyX,X,DemiZone,List) when X < MyX -> %case X < Myx
  if X< MyX - ?radius -> List; %out of the rectangle
    true ->PrevX = ets:prev(Ets,X),
      [{_Key,Value}] = ets:lookup(Ets,X),
    getRobins(Ets,MyX,PrevX,DemiZone,List ++ Value)
  end.