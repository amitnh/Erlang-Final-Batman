%%%-------------------------------------------------------------------
%%% @author amit
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Jul 2020 12:48 PM
%%%-------------------------------------------------------------------
-module(batmanProtocol).
-author("amit").

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).

%%%===================================================================
-record(batmanProtocol_state, {known,pid,seqNum}).
%% pid - the pid of my moveSimulator for putting it inside OGM / MSG
%% -------------------------------------------------------
%% known is a map of known Robins in the system
%% each Robin contains map of neighbors Pid that he received messages from:
%% known ->                {key-> Pid@Node, Value -> {Current Seq Number, Best Link, Last Aware Time, list of neighbors}}
%% list of neighbors ->    {Pid@Node,sorted-list of in window seq numbers, last TTL, last Valid Time}
%%%===================================================================
%% OGM -> {Sequence number, TTL, Originator Address }
%% Originator Address -> {self(),node()}
%%%===================================================================
-define(windowSize, 128). %% Define the size of the window. only sequence numbers in the window will be counted and saved
-define(TTL, 40). %% Time To Live, travel distance of a message
-define(ORIGINATOR_INTERVAL, 1000). %% every ORIGINATOR_INTERVAL the node sends an OGM msg
%%test TODO delete
castPlease(MSG)-> gen_server:cast({global, tal@ubuntu},{test,MSG}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(PidMoveSimulator:: term()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(PidMoveSimulator) ->
  {ok,Pid} = gen_server:start_link(?MODULE, [PidMoveSimulator], []), %{debug,[trace]}
  receive
  after rand:uniform(?ORIGINATOR_INTERVAL) ->  spawn_link(fun()->ogmLoop(Pid) end) % every Robins start sending OGMs after random time up to 1 interval
  end,
  {ok,Pid}. %sends cast to OGM every ORIGINATOR_INTERVAL


ogmLoop(Pid)-> % sends OGM cast to batmanProtocol to send OGM every ?ORIGINATOR_INTERVAL time
  receive
  after ?ORIGINATOR_INTERVAL -> gen_server:cast(Pid,{sendOGM})
  end,
  ogmLoop(Pid).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #batmanProtocol_state{}} | {ok, State :: #batmanProtocol_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([PidMoveSimulator]) ->
  {ok, #batmanProtocol_state{known = maps:new(),pid = PidMoveSimulator,seqNum=0}}. %return a new empty map of known Robins


%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #batmanProtocol_state{}) ->
  {reply, Reply :: term(), NewState :: #batmanProtocol_state{}} |
  {reply, Reply :: term(), NewState :: #batmanProtocol_state{}, timeout() | hibernate} |
  {noreply, NewState :: #batmanProtocol_state{}} |
  {noreply, NewState :: #batmanProtocol_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #batmanProtocol_state{}} |
  {stop, Reason :: term(), NewState :: #batmanProtocol_state{}}).

handle_call({findBestLink, To}, _From, State = #batmanProtocol_state{}) ->
  BestLink = findBestLink(To,State), %finds the best link and returns it
%%  castPlease({batmanNode,node(),myknown, State#batmanProtocol_state.known}),
  {reply, BestLink, State};

handle_call({getKnownFrom}, _From, State = #batmanProtocol_state{}) ->
   Known = State#batmanProtocol_state.known,
   Size =maps:size(Known),
    KnownList = maps:to_list(Known),
  if( Size == 0 ) ->
      {PidTo,NodeTo} = {notfound, notfound};
    true ->
      {{PidTo, NodeTo},_Value} = lists:nth(rand:uniform(Size),KnownList)
  end,
  {reply, {PidTo,NodeTo}, State};

handle_call(_Request, _From, State = #batmanProtocol_state{}) ->
  castPlease({missedCallBatman, request, _Request, from, _From}),
  {reply, ok, State}.






%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #batmanProtocol_state{}) ->
  {noreply, NewState :: #batmanProtocol_state{}} |
  {noreply, NewState :: #batmanProtocol_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #batmanProtocol_state{}}).

handle_cast({sendOGM}, State = #batmanProtocol_state{}) ->
  SeqNum = State#batmanProtocol_state.seqNum +1,
  OGM = {SeqNum, ?TTL,{State#batmanProtocol_state.pid,node()}},
  gen_server:cast(State#batmanProtocol_state.pid,{sendToNeighbors,OGM}),
  {noreply, State#batmanProtocol_state{seqNum = SeqNum}};



%%===============OGM recieved from direct neighbor=================================
handle_cast({ogm,{SeqNum, TTL,OriginatorAddress},OriginatorAddress}, State = #batmanProtocol_state{}) ->
  %----------------------
  % process ogm:
  {NewState,_IsNewSeq,_IsSameTTL} = processOgm(State,{SeqNum, TTL,OriginatorAddress},OriginatorAddress),
  %----------------------
  % rebroadcast ogm:
  Pid=State#batmanProtocol_state.pid,
  if ((OriginatorAddress =/= {Pid,node()}) and (TTL-1 > 0)) -> %% check if im not the Originator. and if TTL>0
    OGM = {SeqNum, TTL-1 ,OriginatorAddress}, % Time to live -1
    gen_server:cast(State#batmanProtocol_state.pid,{sendToNeighbors,OGM}),
    {noreply, NewState};
  true -> {noreply, NewState}
    end;

%%===============OGM recieved not from direct neighbor=================================
handle_cast({ogm,{SeqNum, TTL,OriginatorAddress},FromAddress}, State = #batmanProtocol_state{}) ->
  %----------------------
  % process ogm:
  {NewState,IsNewSeq,IsSameTTL} = processOgm(State,{SeqNum, TTL,OriginatorAddress},FromAddress),
  %----------------------
  % rebroadcast ogm:
  MyAddr = {State#batmanProtocol_state.pid,node()},

  if ((OriginatorAddress =/= MyAddr) and (TTL-1 > 0)) -> %% check if im not the Originator. and if TTL>0
    {_CurrentSeqNumber, BestLink, _LastAwareTime, _ListOfNeighbors} = maps:get(OriginatorAddress,NewState#batmanProtocol_state.known),

      if (BestLink =:= FromAddress) -> % i rebroadcast only if i received the msg from Best link
              if (IsNewSeq or IsSameTTL) ->
                        OGM = {SeqNum, TTL-1 ,OriginatorAddress}, % Time to live -1
                        gen_server:cast(State#batmanProtocol_state.pid,{sendToNeighbors,OGM});
                true-> ok
              end;
      true-> ok
      end;
  true-> ok
  end,
  {noreply, NewState};

%Msg failed: delete neighbor
handle_cast({deleteNeighbor, ToDelete,AddressFrom,Msg},State = #batmanProtocol_state{}) ->
NewKnown = deleteNeighbor(ToDelete,AddressFrom,State#batmanProtocol_state.known),
{noreply, State#batmanProtocol_state{known = NewKnown}};

handle_cast({deleteBatman, AddressFrom}, State = #batmanProtocol_state{}) -> %case the batman has no neighbors
  Known = State#batmanProtocol_state.known,
  {noreply, State#batmanProtocol_state{known = maps:remove(AddressFrom,Known)}};


handle_cast(Request, State = #batmanProtocol_state{}) ->
  castPlease({missedMessegBATMAN,request,Request}),

  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #batmanProtocol_state{}) ->
  {noreply, NewState :: #batmanProtocol_state{}} |
  {noreply, NewState :: #batmanProtocol_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #batmanProtocol_state{}}).
handle_info(_Info, State = #batmanProtocol_state{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #batmanProtocol_state{}) -> term()).
terminate(_Reason, _State = #batmanProtocol_state{}) ->
  ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================



%% known ->                Request{key-> Pid@Node, Value -> {Current Seq Number, Best Link, Last Aware Time, list of neighbors}}
%% list of neighbors ->    {Pid@Node,sorted-list of in window seq numbers, last TTL, last Valid Time}


%%  processOgm -> receive the OGM and updates the know map accordingly (folowing the BATMAN protocol)
%%  returns -> {NewState,IsNewSeq,IsSameTTL}
processOgm(State, {SeqNum, TTL,OriginatorAddress},FromAddress) ->
  %first thing to check if the SeqNum is new, if not return State
  Known = State#batmanProtocol_state.known,
  IsKnown= maps:is_key(OriginatorAddress,Known),
 if IsKnown->
   %=====================in the map, update it=================
   KnownBatman = maps:get(OriginatorAddress,Known),
   {CurrentSeqNumber, BestLink, LastAwareTime, ListOfNeighbors} = KnownBatman,
   Neighbor = getNeighbor(FromAddress,ListOfNeighbors,TTL), %returs the neighbor, or a new neighbor with an empty SeqList
   {_FromAdd,SeqList, LastTTL, LastValidTime} =Neighbor,
     if SeqList == [] -> % not a neighbor (yet)
        if CurrentSeqNumber < SeqNum ->% a new *Current* Seq Num
          NewKnowBatman = updateCurrSeqNum(KnownBatman,FromAddress,SeqNum,lastTTL), % return a new KnownBatman with everthing updated
          {State#batmanProtocol_state{known = maps:put(OriginatorAddress,NewKnowBatman,Known)},true,false};
        true-> % not a new *Current* Seq Num
         NewKnowBatman = {CurrentSeqNumber, BestLink, erlang:system_time(millisecond), ListOfNeighbors ++ [{FromAddress,[SeqNum], TTL, erlang:system_time(millisecond)}]},
         {State#batmanProtocol_state{known = maps:put(OriginatorAddress,NewKnowBatman,Known)},true,false}
        end;
     true-> % neighbor already exists
        IsNewSeq = not (lists:member(SeqNum,SeqList)),
        if  IsNewSeq -> % a new Seq Num
            if (CurrentSeqNumber < SeqNum) ->% a new *Current* Seq Num
              Batman = updateCurrSeqNum(KnownBatman,FromAddress,SeqNum,TTL), % return a new KnownBatman with everthing updated
              NewKnowBatman = updateBestLink(Batman,OriginatorAddress), % change the best link
              if (NewKnowBatman == deleteBatman) ->% if the neighbors list is empty
                {State#batmanProtocol_state{known = maps:remove(OriginatorAddress,Known)},false,false};
              true-> % if the neighbors list is not empty, (i have a best link)
                {State#batmanProtocol_state{known = maps:put(OriginatorAddress,NewKnowBatman,Known)},true,false}
                end;
            true->  % not a new Current Seq Num *but* a new SeqNum

              Batman = addSeqNum(KnownBatman,FromAddress,SeqNum,lastTTl), % return a new KnownBatman with everthing updated
              NewKnowBatman = updateBestLink(Batman,OriginatorAddress), % change the best link
              if (NewKnowBatman == deleteBatman) ->% if the neighbors list is empty
                {State#batmanProtocol_state{known = maps:remove(OriginatorAddress,Known)},false,false};
                true-> % if the neighbors list is not empty, (i have a best link)
                  {State#batmanProtocol_state{known = maps:put(OriginatorAddress,NewKnowBatman,Known)},true,false}
              end
            end;
        true-> %not a new Seq -> no process ! but we check if the LastTTL == TTL for Rebroadcasting later
          {State#batmanProtocol_state{},false,LastTTL == TTL}
        end
       end;
  %=====================knownBatman not in the map, generate it=================
  true ->
    NewKnown = maps:put(OriginatorAddress,{SeqNum,FromAddress,erlang:system_time(millisecond),[{FromAddress,[SeqNum],TTL,erlang:system_time(millisecond) }]},Known),
    {State#batmanProtocol_state{known = NewKnown},true,false} % IsNewSeq = true,IsSameTTL = false,
  end.




%================================================================
%================================================================
%=====================Aid funcitions=============================
%================================================================
%================================================================

% getNeighbor(FromAddress,ListOfNeighbors,TTL) -> neighbor
%returs the neighbor, or a new neighbor with an empty SeqList
%% list of neighbors ->    {Pid@Node,sorted-list of in window seq numbers, last TTL, last Valid Time}
getNeighbor(FromAddress, [],TTL) -> {FromAddress,[],TTL,erlang:system_time(millisecond)};
getNeighbor(FromAddress, [{FromAddress,SeqList,LastTTL,LastValidTime}|_],_) ->{FromAddress,SeqList,LastTTL,LastValidTime};
getNeighbor(FromAddress, [_|ListOfNeighbors],TTL) -> getNeighbor(FromAddress, ListOfNeighbors,TTL).



% return a new KnownBatman with everything updated (uses addSeqNum)
updateCurrSeqNum({_CurrentSeqNumber, BestLink, LastAwareTime, ListOfNeighbors}, FromAddress, SeqNum,NewTTL) ->

  UpdatedListOfNeighbors = updateInWindow(ListOfNeighbors,SeqNum),
  {_,_,_,NewListOfNeighbors} = addSeqNum({SeqNum, BestLink, LastAwareTime, UpdatedListOfNeighbors},FromAddress, SeqNum,NewTTL), %updates in window lists and adds the SeqNum to FromAddress
  {SeqNum, getBestLink(ListOfNeighbors,BestLink,0), erlang:system_time(millisecond), NewListOfNeighbors}.

%returns lists of neighbors in the new in-window
updateInWindow(ListOfNeighbors, CurrSeqNum) ->NewLists = [{FromAddress,lists:filter(fun(SeqNum) -> (SeqNum > (CurrSeqNum- ?windowSize)) end, SeqList), LastTTL, LastValidTime}||{FromAddress,SeqList, LastTTL, LastValidTime}<-ListOfNeighbors],
  [{FromAddress,SeqList, LastTTL, LastValidTime}||{FromAddress,SeqList, LastTTL, LastValidTime}<-NewLists,(length(SeqList)>0)]. % removes the empty neighbors




% return a new KnownBatman with SeqNum added to the Neighbor
%Batman -> {Current Seq Number, Best Link, Last Aware Time, list of neighbors}}
%if you dont want to change the TTL put in NewTTL->lastTTl
addSeqNum({CurrentSeqNumber, BestLink, LastAwareTime, ListOfNeighbors}, FromAddress, SeqNum,NewTTL) ->
  Neighbor= getNeighbor(FromAddress,ListOfNeighbors,ttlNotRelevant),
  NewNeighbor =addSeqNumtoNeighbor(Neighbor,SeqNum,NewTTL),

  NewListOfNeighbors = lists:delete(Neighbor,ListOfNeighbors) ++ [NewNeighbor],
  {CurrentSeqNumber, BestLink, LastAwareTime, NewListOfNeighbors}.
addSeqNumtoNeighbor({FromAddress,SeqList,LastTTL,_LastValidTime}, SeqNum,lastTTl)  ->{FromAddress,lists:sort(SeqList ++ [SeqNum]),LastTTL,erlang:system_time(millisecond)};
addSeqNumtoNeighbor({FromAddress,SeqList,_LastTTL,_LastValidTime}, SeqNum,NewTTL)  ->{FromAddress,lists:sort(SeqList ++ [SeqNum]),NewTTL,erlang:system_time(millisecond)}.



%returns Batman with updated Best Link
%Batman -> {Current Seq Number, Best Link, Last Aware Time, list of neighbors}}
%% list of neighbors ->    {Pid@Node,sorted-list of in window seq numbers, last TTL, last Valid Time}
updateBestLink({CurrentSeqNumber, BestLink, LastAwareTime, ListOfNeighbors},BatmanAdd) ->
  IsNeighbor = lists:member(BatmanAdd,ListOfNeighbors),
  if IsNeighbor -> %if the the ToBatman is acctually my neighbor -> he is the best link
    {CurrentSeqNumber, BatmanAdd, LastAwareTime, ListOfNeighbors};
  true-> % case is not my neighbor-> search for the biggest in-window list
    if (ListOfNeighbors == []) ->  deleteBatman;
    true-> {CurrentSeqNumber, getBestLink(ListOfNeighbors,BestLink,0), LastAwareTime, ListOfNeighbors}
    end
  end.

getBestLink([],BestLink,_) ->BestLink;
getBestLink([{FromAddress,SeqList,_LastTTL,_LastValidTime}|ListOfNeighbors],BestLink,PacketSize) ->
    if length(SeqList)>PacketSize -> getBestLink(ListOfNeighbors,FromAddress,length(SeqList));
      true ->getBestLink(ListOfNeighbors,BestLink,PacketSize)
    end.



%returns the best link to "To", if it doesn't exists it return an atom: "iDontKnowHim"
findBestLink(To, State) ->
  Known = State#batmanProtocol_state.known,
  try maps:get(To,Known) of
    {_CurrentSeqNumber, BestLink, _LastAwareTime, _ListOfNeighbors} ->BestLink
  catch
    _:{badkey,_key} -> iDontKnowHim;
    _:{badmap,_map} -> badmap;
    _:_ -> errorInfindBestLink
  end.

% (list of neighbors ->    {Pid@Node,sorted-list of in window seq numbers, last TTL, last Valid Time})
% deletes Neighbor from KnownBatman in Known, and returns a NewKnown without this Neighbor in KnownBatman
deleteNeighbor(ToDelete,AddressFrom,Known) ->
  {CurrentSeqNumber, BestLink, LastAwareTime, ListOfNeighbors}= maps:get(AddressFrom,Known),
  NewList = deleteNeighborFromList(ListOfNeighbors,ToDelete), %deletes the Neighbor from list
  NewKnownBatman =  {CurrentSeqNumber, BestLink, LastAwareTime, NewList},% making new KnownBatman with the new list

  NewKnowBatman = updateBestLink(NewKnownBatman,AddressFrom),
  if (NewKnowBatman == deleteBatman) ->% if the neighbors list is empty
    gen_server:cast(self(), {deleteBatman, AddressFrom});
    true -> % if the neighbors list is not empty, (i have a best link)
        maps:put(AddressFrom,NewKnowBatman,Known)
  end. %updates the BestLink in KnownBatman and put it in known (a NewKnown is returned)



deleteNeighborFromList(ListOfNeighbors, Neighbor) ->
  deleteNeighborFromList(ListOfNeighbors, Neighbor,[]).
deleteNeighborFromList([{Neighbor,_,_,_}|ListOfNeighbors], Neighbor,NewList)-> NewList ++ ListOfNeighbors; %if i found the Neighbor put it out of the newList
deleteNeighborFromList([H|ListOfNeighbors], Neighbor,NewList)-> deleteNeighborFromList(ListOfNeighbors,Neighbor,NewList++[H]).% if its not the Neighbor add it to the new List

