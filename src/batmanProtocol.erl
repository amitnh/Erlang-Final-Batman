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
  {ok,Pid} = gen_server:start_link(?MODULE, [PidMoveSimulator], [{debug,[trace]}]),
  spawn_link(fun()->ogmLoop(Pid) end). %sends cast to OGM every ORIGINATOR_INTERVAL


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
handle_call(_Request, _From, State = #batmanProtocol_state{}) ->
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
  NewState = processOgm(State,{SeqNum, TTL,OriginatorAddress},OriginatorAddress),
  %----------------------
  % rebroadcast ogm:
  if (OriginatorAddress /= {State#batmanProtocol_state.pid,node()}) and (TTL-1 > 0) -> %% check if im not the Originator. and if TTL>0
    OGM = {SeqNum, TTL-1 ,OriginatorAddress}, % Time to live -1
    gen_server:cast(State#batmanProtocol_state.pid,{sendToNeighbors,OGM})
    end,

  {noreply, NewState};
%%===============OGM recieved not from direct neighbor=================================
handle_cast({ogm,{SeqNum, TTL,OriginatorAddress},FromAddress}, State = #batmanProtocol_state{}) ->
  %----------------------
  % process ogm:
  {NewState,IsNewSeq,IsSameTTL} = processOgm(State,{SeqNum, TTL,OriginatorAddress},FromAddress),
  %----------------------
  % rebroadcast ogm:
  if (OriginatorAddress /= {State#batmanProtocol_state.pid,node()}) and (TTL-1 > 0) -> %% check if im not the Originator. and if TTL>0
    {CurrentSeqNumber, BestLink, LastAwareTime, ListOfNeighbors} = maps:get(OriginatorAddress,NewState#batmanProtocol_state.known),
      if BestLink == FromAddress -> % i rebroadcast only if i received the msg from Best link
              if IsNewSeq or IsSameTTL ->
                        OGM = {SeqNum, TTL-1 ,OriginatorAddress}, % Time to live -1
                        gen_server:cast(State#batmanProtocol_state.pid,{sendToNeighbors,OGM})
              end
      end
  end,
  {noreply, NewState};

%%============================================================================================




handle_cast(_Request, State = #batmanProtocol_state{}) ->
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



%% known ->                {key-> Pid@Node, Value -> {Current Seq Number, Best Link, Last Aware Time, list of neighbors}}
%% list of neighbors ->    {Pid@Node,sorted-list of in window seq numbers, last TTL, last Valid Time}

%%  processOgm -> receive the OGM and updates the know map accordingly
%%  returns -> {NewState,IsNewSeq,IsSameTTL}
processOgm(State, {SeqNum, TTL,OriginatorAddress},FromAddress) ->
  %first thing to check if the SeqNum is new, if not return State
  Known = State#batmanProtocol_state.known,
  IsKnown= maps:is_key(OriginatorAddress,Known),

 if IsKnown->
   %=====================in the map, update it=================
   KnownBatman = maps:get(OriginatorAddress,Known),
   {CurrentSeqNumber, BestLink, LastAwareTime, ListOfNeighbors} = KnownBatman,

   Neighbor = getNeighbor(FromAddress,ListOfNeighbors), %returs the neighbor, or a new neighbor with an empty SeqList
   {_FromAdd,SeqList, LastTTL, LastValidTime} =Neighbor,
   if SeqList == [] -> % not a neighbor (yet)
      if CurrentSeqNumber < SeqNum ->% a new *Current* Seq Num
        NewKnowBatman = updateCurrSeqNum(KnownBatman,FromAddress,SeqNum), % return a new KnownBatman with everthing updated
        {State#batmanProtocol_state{known = maps:put(OriginatorAddress,NewKnowBatman,Known)},true,false};
        true-> % not a new *Current* Seq Num
       NewKnowBatman = {CurrentSeqNumber, BestLink, erlang:system_time(millisecond), ListOfNeighbors ++ {FromAddress,[SeqNum], TTL, erlang:system_time(millisecond)}},
       {State#batmanProtocol_state{known = maps:put(OriginatorAddress,NewKnowBatman,Known)},true,false}
      end;
     true-> % neighbor already exists
      IsNewSeq = lists:member(SeqNum,SeqList),
      if IsNewSeq -> % a new Seq Num
        if CurrentSeqNumber < SeqNum ->% a new *Current* Seq Num
          Batman = updateCurrSeqNum(KnownBatman,FromAddress,SeqNum), % return a new KnownBatman with everthing updated
          NewKnowBatman = updateBestLink(Batman), % change the best link
          {State#batmanProtocol_state{known = maps:put(OriginatorAddress,NewKnowBatman,Known)},true,false};
        true->  % not a new Current Seq Num *but* a new SeqNum
          Batman = addSeqNum(KnownBatman,FromAddress,SeqNum), % return a new KnownBatman with everthing updated
          NewKnowBatman = updateBestLink(Batman), % change the best link
          {State#batmanProtocol_state{known = maps:put(OriginatorAddress,NewKnowBatman,Known)},true,false}
          end;
        true-> %not a new Seq -> no process !
        {State#batmanProtocol_state{},false,false}
      end,

     NewKnown = maps:put(OriginatorAddress,NewKnowBatman,Known),
     {State#batmanProtocol_state{known = NewKnown},true,false} % IsNewSeq = true,IsSameTTL = false,
     end;
  %=====================knownBatman not in the map, generate it=================
  true ->
    NewKnown = maps:put(OriginatorAddress,{SeqNum,FromAddress,erlang:system_time(millisecond),[{FromAddress,[SeqNum],TTL,erlang:system_time(millisecond) }]},Known),
    {State#batmanProtocol_state{known = NewKnown},true,false} % IsNewSeq = true,IsSameTTL = false,
  end.
