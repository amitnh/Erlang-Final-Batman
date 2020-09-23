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
-define(updateMainEts, 5). % refresh rate to mainServer EtsRobins


-record(computerStateM_state, {computerNodes,computersArea, myArea}).
%%test TODO delete
castPlease(MSG)-> gen_server:cast({global, tal@ubuntu},{test,MSG}).
%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(List::list()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link([ComputerNodes,ComputersArea]) ->
%%  gen_server:start_link({global, node()}, ?MODULE, [ComputerNodes,ComputersArea],[]),% [{debug,[trace]}]),
  gen_server:start_link({global, node()}, ?MODULE, [ComputerNodes,ComputersArea],[{debug,[trace]}]),
  receive
    after 500-> ok
  end,
  spawn_link(fun()->updateMainServerEts() end).

updateMainServerEts()-> receive
                          after 1000 div ?updateMainEts -> gen_server:cast({global, tal@ubuntu},{etsUpdate,node(),ets:tab2list(etsX),ets:tab2list(etsY)})
                        end, updateMainServerEts().


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #computerStateM_state{}} | {ok, State :: #computerStateM_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([ComputerNodes,ComputersArea]) -> %gets the area of the computer, the borders.
  %the ETS is build like this: [{Location1,[pid1,pid2...]},{Location2,[pid1,pid2...]},....]
  ets:new(etsX,[ordered_set,public,named_table,{read_concurrency, true},{write_concurrency, true}]),
  ets:new(etsY,[ordered_set,public,named_table,{read_concurrency, true},{write_concurrency, true}]),
  MyArea = getArea(ComputerNodes,ComputersArea,node()),
  initRobins(MyArea),   %% spawn N/4 Robins and monitors them
  {ok, #computerStateM_state{computerNodes = ComputerNodes, computersArea = ComputersArea, myArea = MyArea}}. %saves the area border

% gets My Area from lists of nodes and areas
getArea([],[],_)-> areaCantBeFound;
getArea([MyNode|_],[Area|_],MyNode) -> Area;
getArea([_|T1],[_|T2],MyNode) -> getArea(T1,T2,MyNode).

initRobins(MyArea) -> %spawn N/4 Robins
  Loop = lists:seq(1,?N div 4),
  %spawn a Robin and monitor it, we add the DemilitarizedZone, so the moveSimulator will know it
  [spawn(moveSimulator,start_link,[[MyArea,?DemilitarizedZone]])|| _<- Loop].



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

handle_call({sendOGMtoNeighborsY,MyX,MyY,OGM,{Pid,Node}}, _From, State = #computerStateM_state{}) ->% todo todo only temp
  {reply, [], State};


handle_call(_Request, _From, State = #computerStateM_state{}) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #computerStateM_state{}) ->
  {noreply, NewState :: #computerStateM_state{}} |
  {noreply, NewState :: #computerStateM_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #computerStateM_state{}}).

%%===================================================================================
handle_cast({sendOGMtoNeighborsX,MyX,MyY,OGM,{Pid,Node}}, State = #computerStateM_state{}) -> %todo todo only temp
  {StartX,EndX,_,_}= State#computerStateM_state.myArea,
  DisToLeft = MyX - StartX,
  DisToRight = EndX - MyY,
  if DisToRight < DisToLeft ->% im close to the right border
    if EndX < 2000 -> % there is a computer to te right
      [Node] = neighbor(State,right),
      gen_server:cast(Node,{ogmFromNeighbor,MyX,MyY,OGM,{Pid,Node}})
    end;
  true -> % else DisToRight >= DisToLeft
    if StartX > 0 -> % there is a computer to the left
      [Node] = neighbor(State,left),
      gen_server:cast(Node,{ogmFromNeighbor,MyX,MyY,OGM,{Pid,Node}})
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
      gen_server:cast(Node,{ogmFromNeighbor,MyX,MyY,OGM,{Pid,Node}})
    end;
    true -> % else DisToRight >= DisToLeft
      if StartY > 0 -> % there is a computer to the left
        [Node] = neighbor(State,up),
        gen_server:cast(Node,{ogmFromNeighbor,MyX,MyY,OGM,{Pid,Node}})
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