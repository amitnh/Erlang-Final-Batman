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
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, getPidsInCircle/4,
  code_change/3, castPlease/1, robinsInRadiusForRemote/2]).

-define(SERVER, ?MODULE).
-define(updateEts ,20). %how many time per second to update the ETS's
-define(velMax , 20). %range of the random velocity of the node in meter/milisec
-define(timeRange ,{1000,5000}). %range of the random time to change direction of the node in milisec
-define(radius ,500).
-define(DemilitarizedZone, 50). % how much area to add to each computer, "Demilitarized zone".

-record(moveSimulator_state, {startX,endX,startY,endY,demiZone,myX,myY,time,velocity,direction,myBatman,pcPid}).


%%test TODO delete
castPlease(MSG)-> gen_server:cast({global, tal@ubuntu},{test,MSG}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(List::term()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link([Area,DemiZone,PCPid]) ->
  {ok,Pid} = gen_server:start_link( ?MODULE, [Area,DemiZone,PCPid,{0,0,0,0}], []), %TODO change the name ?MODULE, it wont work with more then 1 computer
%%  castPlease(moveSimulatorOnline),
  spawn_link(fun()->etsTimer(Pid) end),
  spawn_link(fun()->vectorTimer(Pid) end);

%if a batman is switching computer
start_link([Area,DemiZone,PCPid,{X,Y,Dir,Vel}]) ->
  {ok,Pid} = gen_server:start_link( ?MODULE, [Area,DemiZone,PCPid,{X,Y,Dir,Vel}], []), %TODO change the name ?MODULE, it wont work with more then 1 computer
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
%%init([{StartX,EndX,StartY,EndY},DemiZone,PCPid]) ->
%%  {ok, MyBatman} = batmanProtocol:start_link(self()), %creates batmanProtocol and link it to this process
%%  {X,Y} = startLocation(StartX,EndX,StartY,EndY), % put my new random location in the etsX and etsY
%%  {ok, #moveSimulator_state{startX = StartX,endX = EndX,startY = StartY,endY = EndY,
%%    demiZone = DemiZone,myX = X,myY = Y,time = erlang:system_time(millisecond),velocity=0,direction=0,myBatman = MyBatman,pcPid = PCPid}};

%if a batman is switching computer with specific X,Y locations
init([{StartX,EndX,StartY,EndY},DemiZone,PCPid,{X,Y,Dir,Vel}]) ->
  {ok, MyBatman} = batmanProtocol:start_link(self()), %creates batmanProtocol and link it to this process

  if( {X,Y,Dir,Vel} == {0,0,0,0} )->%%initiating field
      {Xnew,Ynew} = startLocation(StartX,EndX,StartY,EndY), % put my new random location in the etsX and etsY
      {ok, #moveSimulator_state{startX = StartX,endX = EndX,startY = StartY,endY = EndY,
        demiZone = DemiZone,myX = Xnew,myY = Ynew,time = erlang:system_time(millisecond),velocity=0,direction=0,myBatman = MyBatman,pcPid = PCPid}};

  true ->  %receiving a new batman from another computer
      ListX = listToUpdate(ets:lookup(etsX,X),X),
      ListY = listToUpdate(ets:lookup(etsY,Y),Y),
      ets:insert(etsX,ListX),
      ets:insert(etsY,ListY),
      {ok, #moveSimulator_state{startX = StartX,endX = EndX,startY = StartY,endY = EndY,
        demiZone = DemiZone,myX = X,myY = Y,time = erlang:system_time(millisecond),velocity=Vel,direction=Dir,myBatman = MyBatman,pcPid = PCPid}}
  end;

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
  PCPid = State#moveSimulator_state.pcPid,
  CurrTime = erlang:system_time(millisecond),
  DeltaTime = CurrTime - State#moveSimulator_state.time,
  StartX = State#moveSimulator_state.startX,
  StartY = State#moveSimulator_state.startY,
  EndX = State#moveSimulator_state.endX,
  EndY = State#moveSimulator_state.endY,

  X = X0 + math:cos(Dir * math:pi() / 180)*Vel*DeltaTime/1000, % x = vt , trigo
  Y = Y0 + math:sin(Dir * math:pi() / 180)*Vel*DeltaTime/1000,

  %boarders check:
  if ((X < 0) or (X > 2000) or (Y < 0) or (Y > 2000)) ->
    gen_server:cast(self(),{changeDir});
    true -> ok
  end,
  %if X or Y are out of bounds, need to check if theres a new border or if a terminate is necessary
  if ((X>EndX+?DemilitarizedZone) or (X<StartX-?DemilitarizedZone) or (Y>EndY+?DemilitarizedZone) or (Y<StartY-?DemilitarizedZone)) ->
    try %wait for the computerServer to bring back an answer about the borders
      {NewStartX,NewEndX,NewStartY,NewEndY,ToTerminate} = gen_server:call(PCPid,
            {updateBorders,{X,Y,State#moveSimulator_state.direction,State#moveSimulator_state.velocity}}),
      if ToTerminate ->
          gen_server:stop(State#moveSimulator_state.myBatman,normal,1000),%Moving to another computer, Shut down Batman Server
          gen_server:stop(self(),normal,1000);%Shut down MoveSimulator Server
        true -> gen_server:cast(self(),{updateBorders,NewStartX,NewEndX,NewStartY,NewEndY})
      end
    catch _-> innerConnectionError
    end;
  true -> ok
  end,
  {X,Y,CurrTime}.
%%---------------------------------------------------------------------------------------------------
%%,startX = NewStartX,startY = NewStartY, endX = NewEndX, endY = NewEndY


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

handle_call({receiveMsg,To,Msg}, From, State = #moveSimulator_state{}) ->
  castPlease({receiveMsg,from,From}),
  if (To == {self(),node()}) -> castPlease(Msg); % case the msg is for me -> cast it
    true-> gen_server:cast(self(),{sendMsg,To, {From,node()},Msg}) % case the msg is not for me -> pass it on
  end,
  {reply, sent, State};

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

%%============================================================================================
%% send MSG to someone using the BATMAN Protocol
handle_cast({sendMsg,To,From,Msg}, State = #moveSimulator_state{}) ->
  castPlease({cast, sendMsg,self(), received,To,Msg}),
  MyBatman = State#moveSimulator_state.myBatman,
  BestLink  = gen_server:call(MyBatman, {findBestLink, To}),
  if is_tuple(BestLink) -> % there where no problems, Best Link was found
    {BestLinkPid,BestLinkNode}= BestLink,
    Node = node(),
    try %if the call fails
      if
        (From == BestLink) -> % im not sending the msg back to the one that sent it to me, infinite loop
                                    %1.delete the neighbor 2.send the msg again to the next best link
                                        gen_server:cast(MyBatman, {deleteNeighbor, {BestLinkPid,BestLinkNode},To,Msg});
        (Node == BestLinkNode) -> % if the node is in my pc send it to him directly
          Reply = gen_server:call(BestLinkPid,{receiveMsg,To,Msg}),
          if
            (Reply == sent) -> {noreply, State#moveSimulator_state{}};
            true-> % if neighbor didn't received the msg (Out Of Range / died for some reason) then
                   %1.delete the neighbor 2.send the msg again to the next best link
              gen_server:cast(MyBatman, {deleteNeighbor, BestLink,To,Msg})
          end;
        true -> % if the neighbor is on other computer and not the one that sent me the msg
          PcPid = State#moveSimulator_state.pcPid,
          Reply = gen_server:call(PcPid,{sendMsg,To,BestLinkPid,Msg}),
          if
            (Reply == sent) -> {noreply, State#moveSimulator_state{}};
            true-> % if neighbor didn't received the msg (Out Of Range / died for some reason) then
                     %1.delete the neighbor 2.send the msg again to the next best link
              gen_server:cast(MyBatman, {deleteNeighbor, BestLink,To,Msg})
          end,
              {noreply, State#moveSimulator_state{}}
      end
    catch % if Msg didn't sent
          _-> % if neighbor didn't received the msg (Out Of Range / died for some reason) then
                  %1.delete the neighbor 2.send the msg again to the next best link
                  gen_server:cast(MyBatman, {deleteNeighbor, BestLink,To,Msg}),
        {noreply, State#moveSimulator_state{}}
    end;
  true-> {noreply, State#moveSimulator_state{}} %i dont know "To", cant reach him
end;

%=======================================================================================================

%%============================================================================================
handle_cast({updateBorders,NewStartX,NewEndX,NewStartY,NewEndY}, State = #moveSimulator_state{}) ->
%%  castPlease(updateMovementVector),
  {noreply, State#moveSimulator_state{startX = NewStartX,endX = NewEndX,startY = NewStartY,endY = NewEndY}};

handle_cast({updateMovementVector,CurrTime,Velocity,Direction}, State = #moveSimulator_state{}) ->
%%  castPlease(updateMovementVector),
  {noreply, State#moveSimulator_state{time = CurrTime,velocity = Velocity, direction = Direction}};


handle_cast({changeDir}, State = #moveSimulator_state{}) ->
  Direction = State#moveSimulator_state.direction,
  NewDirection = (0 - Direction),
{noreply, State#moveSimulator_state{direction = NewDirection}};

%updateEts updates the location of my PID in the etsX and etsY
handle_cast({updateEts}, State = #moveSimulator_state{}) ->
  {X,Y,CurrTime} = updatedXYlocations(State), %also checks if the borders are ok or should i terminate and move to another nearby PC
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
    {noreply, State#moveSimulator_state{myX = X,myY = Y,time = CurrTime}};

%%============sendToNeighbors=============================
%% sends OGM to radius pids
handle_cast({sendToNeighbors,OGM}, State = #moveSimulator_state{}) -> % Msg usually is OGM
  ListOfRobins = robinsInRadius(State,OGM),
%%  castPlease({ListOfRobins,{ogm,OGM,{self(),node()}}}),
  [gen_server:cast(Pid,{ogm,OGM,{self(),node()}}) ||{Pid,_Node} <- ListOfRobins], % sends the OGM and the sender address to all the neighbors
  {noreply, State};


%%===============OGM recieved=================================
%%send it to the Batman
handle_cast({ogm,OGM,{Pid,Node}}, State = #moveSimulator_state{}) ->
  Batman =State#moveSimulator_state.myBatman,
  gen_server:cast(Batman,{ogm,OGM,{Pid,Node}}),
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


%%====================================================================
%%robinsInRadius -> return all the {pid,node} of all the pids that in my radius
%%====================================================================
robinsInRadius(State,OGM) ->
  MyX = round(State#moveSimulator_state.myX),
  MyY = round(State#moveSimulator_state.myY),
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
    gen_server:cast({global, tal@ubuntu},{sendOGMtoNeighborsX,MyX,MyY,OGM,{self(),node()}}); % tell the computer to send OGM to the nodes in other computer
  true->ok
    end,
  if (MyY + ?radius > EndY - DemiZone) or (MyY - ?radius > StartY + DemiZone) ->
    gen_server:cast({global, tal@ubuntu},{sendOGMtoNeighborsY,MyX,MyY,OGM,{self(),node()}}); %
  true->ok
    end,

   [Add||Add<-getPidsInCircle(MyX,MyY,Xlist,Ylist),Add /= {self(),node()}]. % deletes myself from neighbors and return the list of pids in my radius
%%===========================================================
%%===========================================================
%%help other computer just find the pids in the radius
robinsInRadiusForRemote(X,Y) ->

  %Xlist and Ylist are all the pids in square of (radius x radius)
  Xlist = getPrev(etsX,X,X,[]) ++ getNext(etsX,X,X,[]),
  Ylist = getPrev(etsY,Y,Y,[]) ++ getNext(etsY,Y,Y,[]),
  getPidsInCircle(Y,Y,Xlist,Ylist).
%%===========================================================




%% getPidsInCircle ->
%% input: my x,y location and 2 lists as follows:
%% Xlist = [{Xlocation,{Pid,node}},....] and Ylist and return only the pids that are in the radius
%%
%% output: is 1 list with all the Addresses in my radius -> [{Pid1,Node1},{Pid2,Node1},{Pid3,Node2},...]
%%===========================================================
getPidsInCircle(X,Y,Xlist,Ylist)->

  Square = getSquare(Xlist,Ylist),
  Circle =getCircle(X,Y,Square), % Square -> [{x,y,address},...]
  [Address||{_X,_Y,Address}<-Circle]. % returns only the Addresses back

%%getSquare returns {x,y,address},...] withing a square of radiusXradius
getSquare(Xlist,Ylist) -> getSquare(Xlist,Ylist,[]).
getSquare([],_,List)-> lists:filter(fun(X) -> X /= {} end, List); %removes all empty tuples from the list
%for each X we will lookup a similar Y withing the square
getSquare([{X,Xaddr}|T],Ylist,List) -> getSquare(T,Ylist,[getY(X,Xaddr,Ylist)] ++ List).

getY(_,_,[])-> {};
getY(X,Xaddr,[{Y,Xaddr}|_])->{X,Y,Xaddr};  %found a member in Ylist with the same address as X address
getY(X,Xaddr,[_H|T])->getY(X,Xaddr,T);
getY(_,_,_)-> {}.

getCircle(MyX,MyY,Square) -> R = ?radius,
  [{X,Y,Add}||{X,Y,Add}<-Square, ((X-MyX)*(X-MyX) + (Y-MyY)*(Y-MyY)) < R*R].% Square -> [{x,y,address},...]
%%  lists:filter(fun({X,Y,_Address}) -> (((X*X)+(Y*Y))< (?radius*?radius)) end, Square).
%%===========================================================


%%===========================================================
%%% getNext and getPrev works on etsX or etsY
%% they return a list of Location and pids, in the same computerServer in the radius range
%%===========================================================
getNext(_,_,'$end_of_table',List)-> List;
getNext(_,MyX,NextX,List) when NextX - MyX > ?radius ->  List;  % case nextX not in range
getNext(Ets,MyX,NextX,List)  -> % case nextX in range
  [{_Key,Pids}]= ets:lookup(Ets,NextX), % Value contains the list of Pids
    NewList = List ++ [{NextX,{Pid,node()}}||Pid<-Pids],
    getNext(Ets,MyX,ets:next(Ets,NextX),NewList).

getPrev(_,_,'$end_of_table',List)-> List;
getPrev(_,MyX,PrevX,List) when MyX - PrevX > ?radius ->  List;  % case prevX not in range
getPrev(Ets,MyX,PrevX,List)  -> % case prevX in range
  [{_Key,Pids}]= ets:lookup(Ets,PrevX), % Value contains the list of Pids
  NewList =  [{PrevX,{Pid,node()}}||Pid<-Pids] ++ List,
  getPrev(Ets,MyX,ets:prev(Ets,PrevX),NewList).
%%===========================================================