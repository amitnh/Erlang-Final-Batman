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
-define(updateEts ,30). %how many time per second to update the ETS's
-define(velMax , 3). %range of the random velocity of the node in meter/milisec
-define(timeRange ,{1000,5000}). %range of the random time to change direction of the node in milisec

-record(moveSimulator_state, {startX,endX,startY,endY,myX,myY,time,velocity,direction}).


%%test TODO delete
castPlease(MSG)-> gen_server:cast({global, tal@ubuntu},{test,MSG}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(Area::term()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link([Area]) ->
  {ok,Pid} = gen_server:start_link( ?MODULE, [Area], []), %TODO change the name ?MODULE, it wont work with more then 1 computer
  castPlease(moveSimulatorOnline),
  spawn_link(fun()->etsTimer(Pid) end),
  spawn_link(fun()->vectorTimer(Pid) end),
  test().

test()->
  castPlease(ets:tab2list(etsX)),
  castPlease(ets:tab2list(etsY)),
        receive
         after 2000-> test()
         end.

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
init([{StartX,EndX,StartY,EndY}]) ->
  batmanProtocol:start_link(), %creates batmanProtocol and link it to this process
  {X,Y} = startLocation(StartX,EndX,StartY,EndY), % put my new random location in the etsX and etsY
  {ok, #moveSimulator_state{startX = StartX,endX = EndX,startY = StartY,endY = EndY,myX = X,myY = Y,time = erlang:system_time(millisecond),velocity=0,direction=0}};
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
  castPlease(values),
  castPlease(X0),
  castPlease(Y0),
  castPlease(Vel),
  castPlease(Dir),
  castPlease(DeltaTime),

  X = X0 + math:cos(Dir * math:pi() / 180)*Vel*DeltaTime/1000, % x = vt , trigo
  Y = Y0 + math:sin(Dir * math:pi() / 180)*Vel*DeltaTime/1000,
  castPlease(X),
  castPlease(Y),
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
  castPlease(updateMovementVector),
  castPlease(CurrTime),
  castPlease(Velocity),
  castPlease(Direction),
  {noreply, State#moveSimulator_state{time = CurrTime,velocity = Velocity, direction = Direction}};

%updateEts updates the location of my PID in the etsX and etsY
handle_cast({updateEts}, State = #moveSimulator_state{}) ->
  {X,Y,CurrTime} = updatedXYlocations(State),
  RoundedX = round(State#moveSimulator_state.myX), % X in ets is rounded. not in myX record
  RoundedY = round(State#moveSimulator_state.myY),
  [{OldX,ListX}] = ets:lookup(etsX,RoundedX),
  [{OldY,ListY}] = ets:lookup(etsY,RoundedY),
%%  castPlease(ListX), %[<12630.114.0>] ->[{Location,[self()]}]

  OldListX = ListX -- [self()],% remove the pid from the old location
  OldListY = ListY -- [self()],
  if (length(OldListX) > 0) ->  ets:insert(etsX,[{OldX,OldListX}]);%put back the old Locations lists if not empty
    true-> ets:delete(etsX,OldX) % delete if empty
  end, %
  if (length(OldListY) > 0) ->  ets:insert(etsY,[{OldY,OldListY}]);%put back the old Locations lists if not empty
    true-> ets:delete(etsY,OldY) % delete if empty
  end,

  [{_,NewListX}] = listToUpdate(ets:lookup(etsX,RoundedX),RoundedX),
  [{_,NewListY}] = listToUpdate(ets:lookup(etsY,RoundedY),RoundedY),
  ets:insert(etsX,[{RoundedX,NewListX}]), %insert the new Locations lists
  ets:insert(etsY,[{RoundedY,NewListY}]),
  {noreply, State#moveSimulator_state{myX = X,myY = Y,time = CurrTime}}; % todo check if it works


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
