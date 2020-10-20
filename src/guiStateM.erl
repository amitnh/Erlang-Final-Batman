%%-------------------------------------------------------------------
%%% @author amit and kapelnik
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Aug 2020 15:07
%%%-------------------------------------------------------------------
-module(guiStateM).
-author("amit and kapelnik").

-behaviour(gen_statem).

%% API
-export([start_link/1]).

%% gen_statem callbacks
-export([init/1, format_status/2, state_name/3, handle_event/4, terminate/3,
  code_change/4, callback_mode/0, waiting/3, paint/3, handle_sync_event/3]).
-include_lib("wx/include/wx.hrl").
-define(Height, 500).
-define(Width, 500).
-define(Green,?wxGREEN ).
-define(SERVER, ?MODULE).
-record(guiStateM_state,
{
  mainServer,
  frame,
  env,
  panel,
  text,
  canvas,
  nodesList,  %#[{Node,Color} , {node, Color },,,]
  bitmap,
  dc,
  liveStats,
  numOfProcesses,
  sliders,
  demi
}).
%%-record(guiStateM_state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
start_link([ComputerNodes, MainServerNode]) ->
  {ok,Pid} = gen_statem:start_link({global, ?SERVER}, ?MODULE, [ComputerNodes,MainServerNode], []),
%%  spawn_link(fun()-> refreshtimer(Pid) end),
  Pid.


%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%% @private
%% @doc Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
init([ComputerNodes,MainServerNode]) ->
  %etsRobins:
  % {Pid,Node} -> {X,Y}, {{<0.112.0>,tal@ubuntu},X,Y}
%%  ets:new(etsRobins,[set,public,named_table]),
%%  ets:insert(etsRobins, [{pid1,{200,60}}, {pid2,{300,20}}, {pid3,{500,1000}}, {pid4,{40,800}}]),

  %Initiate WX enviorment and update all necessary in guiState
  Length = length(ComputerNodes),
  Seq = lists:seq(1,Length),
  Env = wx:new(),     %%create a wx
  wx_object:start(?MODULE, [Env], []),
  NodesList = [{lists:nth(N,ComputerNodes),lists:nth(N,[?wxGREEN,?wxBLUE,?wxRED,?wxCYAN])}||N<-Seq],

  F = wxFrame:new(wx:null(), -1, "B.A.T.M.A.N Display", [{size, {?Width+600,?Height+100}}]),  %Creates the main frame for the gui
  P = wxPanel:new(F, [{size, {?Width,?Height-100}}]), % a panel we will split with sizers

  %Create a canvas to paint on later
  C = wxPanel:new(P, [{style, ?wxFULL_REPAINT_ON_RESIZE},{size, {?Height+200,?Width+200}}]), %the main canvas to print on the points

  %Create Sizers to arrange all wxWidget nicely on the panel, and labels
  MainSizer = wxBoxSizer:new(?wxVERTICAL),      %main sizer for alignment within the panel
  Sizer = wxStaticBoxSizer:new(?wxVERTICAL, P, [{label, "B.A.T.M.A.N Field"}]), %inside frame for batman protocol display
  T = wxStaticText:new(P, -1, "Click to Start",[]),
  LiveStats = wxStaticText:new(P, -1, "Number of Processes: ",[]),
  Radiuslabel = wxStaticText:new(P, -1, "Radius:",[]),
  NumofRoblabel = wxStaticText:new(P, -1, "Number of Batmans: ",[]),
  Demilabel = wxStaticText:new(P, -1, "Demiliterized Zone: ",[]),
  OGMlabel = wxStaticText:new(P, -1, "Originator Interval: ",[]),
  MaxVellabel = wxStaticText:new(P, -1, "Max Velocity: ",[]),
  WindowSizelabel = wxStaticText:new(P, -1, "Window Size: ",[]),
  TTLlabel = wxStaticText:new(P, -1, "TTL: ",[]),

  %create a botton to initiate batman protocol and connect it to its event handler
  B = wxButton:new(P, 0, [{label, "Start"}, {size, {150, 50}}]),
  Bapply = wxButton:new(P, 1, [{label, "Apply"}, {size, {150, 50}}]),
  wxButton:connect(B, command_button_clicked, [{callback, fun handle_click/2}, {userData, #{canvas => C,text => T, env => wx:get_env()}}]),
  wxButton:connect(Bapply, command_button_clicked, [{callback, fun handle_click/2}, {userData, #{canvas => C,text => T, env => wx:get_env()}}]),
  %Connect the Panel to paint event
  wxPanel:connect(P, paint, [callback]),

  %Create Sliders for the user to control on all BATMAN protocol specs
  SliderRadius = wxSlider:new(P, 1, 300, 1, 3000,
    [{style, ?wxSL_HORIZONTAL bor
      ?wxSL_LABELS}]),
  SliderNumofRobins = wxSlider:new(P, 1, 20, 4, 500,
    [{style, ?wxSL_HORIZONTAL bor
      ?wxSL_LABELS}]),
  SliderDemiZone = wxSlider:new(P, 1, 50, 0, 1000,
    [{style, ?wxSL_HORIZONTAL bor
      ?wxSL_LABELS}]),
  SliderOGMTime = wxSlider:new(P, 1, 1000, 100, 5000,%in miliseconds
    [{style, ?wxSL_HORIZONTAL bor
      ?wxSL_LABELS}]),
  SliderMaxVelocity = wxSlider:new(P, 1, 10, 1, 1000,
    [{style, ?wxSL_HORIZONTAL bor
      ?wxSL_LABELS}]),
  SliderWindowSize = wxSlider:new(P, 1, 128, 10, 1000,
    [{style, ?wxSL_HORIZONTAL bor
      ?wxSL_LABELS}]),
  SliderTTL = wxSlider:new(P, 1, 30, 10, 100,
    [{style, ?wxSL_HORIZONTAL bor
      ?wxSL_LABELS}]),

  Sliders = {SliderRadius,SliderNumofRobins, SliderDemiZone,SliderOGMTime,SliderMaxVelocity,SliderWindowSize,SliderTTL},

  %Arrange the Frame:
  Sizer1 = wxGridSizer:new(0,1,3,3),
  wxSizer:addSpacer(Sizer, 5),
  wxSizer:add(Sizer, T, [{border, 5}, {flag, ?wxALL}]),

  Sizer2 = wxGridSizer:new(0,2,3,3),
  Sizer3 = wxGridSizer:new(0,2,3,3),
  Sizer4 = wxGridSizer:new(0,2,5,5),
  wxSizer:add(Sizer2, B, [{border, 5}, {flag, ?wxALL}]),

  wxSizer:add(Sizer2, Bapply, [{border, 5}, {flag, ?wxALL}]),

  %For each Slider, add a box with a label
  RadiusSizer = wxStaticBoxSizer:new(?wxVERTICAL, P),
  wxSizer:add(RadiusSizer,Radiuslabel, [{flag, ?wxEXPAND}]),
  wxSizer:add(RadiusSizer,SliderRadius, [{flag, ?wxEXPAND}]),
  wxSizer:add(Sizer2, RadiusSizer, [{flag, ?wxEXPAND}]),

  NumofRobinsSizer = wxStaticBoxSizer:new(?wxVERTICAL, P),
  wxSizer:add(NumofRobinsSizer,NumofRoblabel, [{flag, ?wxEXPAND}]),
  wxSizer:add(NumofRobinsSizer,SliderNumofRobins, [{flag, ?wxEXPAND}]),
  wxSizer:add(Sizer2, NumofRobinsSizer, [{flag, ?wxEXPAND}]),

  DemiZoneSizer = wxStaticBoxSizer:new(?wxVERTICAL, P),
  wxSizer:add(DemiZoneSizer,Demilabel, [{flag, ?wxEXPAND}]),
  wxSizer:add(DemiZoneSizer,SliderDemiZone, [{flag, ?wxEXPAND}]),
  wxSizer:add(Sizer2, DemiZoneSizer, [{flag, ?wxEXPAND}]),

  OGMSizer = wxStaticBoxSizer:new(?wxVERTICAL, P),
  wxSizer:add(OGMSizer,OGMlabel, [{flag, ?wxEXPAND}]),
  wxSizer:add(OGMSizer,SliderOGMTime, [{flag, ?wxEXPAND}]),
  wxSizer:add(Sizer2, OGMSizer, [{flag, ?wxEXPAND}]),

  MaxVelocitySizer = wxStaticBoxSizer:new(?wxVERTICAL, P),
  wxSizer:add(MaxVelocitySizer,MaxVellabel, [{flag, ?wxEXPAND}]),
  wxSizer:add(MaxVelocitySizer,SliderMaxVelocity, [{flag, ?wxEXPAND}]),
  wxSizer:add(Sizer2, MaxVelocitySizer, [{flag, ?wxEXPAND}]),

  WindowSizer = wxStaticBoxSizer:new(?wxVERTICAL, P),
  wxSizer:add(WindowSizer,WindowSizelabel, [{flag, ?wxEXPAND}]),
  wxSizer:add(WindowSizer,SliderWindowSize, [{flag, ?wxEXPAND}]),
  wxSizer:add(Sizer2, WindowSizer, [{flag, ?wxEXPAND}]),

  TTLSizer = wxStaticBoxSizer:new(?wxVERTICAL, P),
  wxSizer:add( TTLSizer, TTLlabel, [{flag, ?wxEXPAND}]),
  wxSizer:add( TTLSizer,SliderTTL, [{flag, ?wxEXPAND}]),
  wxSizer:add(Sizer2, TTLSizer, [{flag, ?wxEXPAND}]),

  %Arrange wxGrid -
  wxSizer:add(Sizer2, LiveStats, [{border, 5}, {flag, ?wxALL}]),
  wxSizer:add(Sizer3, C, [{flag, ?wxEXPAND}, {proportion, 1}]),
  wxSizer:add(Sizer4, Sizer2),
  wxSizer:add(Sizer4, Sizer3),
  wxSizer:add(Sizer1, Sizer4),
  wxSizer:addSpacer(Sizer, 5),
  wxSizer:add(Sizer, Sizer4),

  %add all sizer to the main sizer connected to the panel
  wxSizer:add(MainSizer, Sizer, [{flag, ?wxEXPAND}, {proportion, 1}]),
  wxPanel:setSizer(P, MainSizer),
  wxSizer:layout(MainSizer),

  %Frame is ready for display, update the guiState and go to state waiting
  wxFrame:show(F),
  {ok, waiting, #guiStateM_state{mainServer = MainServerNode, env = Env, canvas = C,frame = F,panel = P,
        text = T, nodesList = NodesList,liveStats = LiveStats, sliders = Sliders, demi = 50,
    numOfProcesses = 0}}.




%% @private
%% @doc This function is called by a gen_statem when it needs to find out
%% the callback mode of the callback module.
callback_mode() ->
state_functions.

%% @private
%% @doc Called (1) whenever sys:get_status/1,2 is called by gen_statem or
%% (2) when gen_statem terminates abnormally.
%% This callback is optional.
format_status(_Opt, [_PDict, _StateName, _State]) ->
  Status = some_term,
  Status.

%% @private
%% @doc There should be one instance of this function for each possible
%% state name.  If callback_mode is state_functions, one of these
%% functions is called when gen_statem receives and event from
%% call/2, cast/2, or as a normal process message.

state_name(star, _EventContent, State = #guiStateM_state{}) ->
  NextStateName = next_state,
  {next_state, NextStateName, State}.

%Waiting for the user to hit a button to start. next state - paint
waiting(cast,paintnow,State = #guiStateM_state{}) ->
  {next_state, paint, State};

%when the user hits apply, start drawing. next state - paint
waiting(cast, {sendnewStats,_Env},State = #guiStateM_state{}) ->
  gen_statem:cast({global, ?SERVER}, paintnow),
  {next_state, paint, State};

waiting(cast, _, State = #guiStateM_state{}) ->
  {next_state, waiting, State}.

%paint all ETSrobin on the screen and update the spec lable,next state - paint
paint(cast, {refresh,ETS}, State = #guiStateM_state{}) ->
  do_refresh(State,ETS),
  {next_state, paint, State};

%update numberofProcesses in the guiState, next state - paint
paint(cast, {refreshProcesses,NumOfProcesses}, State = #guiStateM_state{}) ->
{next_state, paint, State#guiStateM_state{numOfProcesses = NumOfProcesses}};


%Sliders = {SliderRadius,SliderNumofRobins, SliderDemiZone,SliderOGMTime},
%send to mainServer the new stats the user inserted in sliders
%Read the sliders new info, and send the new specs to the Main Server. next state - painting
paint(cast, {sendnewStats,Env}, State = #guiStateM_state{sliders = Sliders}) ->
  wx:set_env(Env),
  {SliderRadius,SliderNumofRobins, SliderDemiZone,SliderOGMTime,SliderMaxVelocity,SliderWindowSize,SliderTTL} = Sliders,
  Radius = wxSlider:getValue(SliderRadius),
  NumofRobins = wxSlider:getValue(SliderNumofRobins),
  DemiZone = wxSlider:getValue(SliderDemiZone),
  ORIGINATOR_INTERVAL = wxSlider:getValue(SliderOGMTime),
  MaxVelocity =wxSlider:getValue(SliderMaxVelocity),
  WindowSize = wxSlider:getValue(SliderWindowSize),
  TTL = wxSlider:getValue(SliderTTL),
  MainServer = State#guiStateM_state.mainServer,
  moveSimulator:castPlease({sendingsliders,{Radius,NumofRobins,DemiZone,ORIGINATOR_INTERVAL,MaxVelocity,WindowSize,TTL}}),
  gen_server:cast({global,MainServer},{newStats, {Radius,NumofRobins,DemiZone,ORIGINATOR_INTERVAL,MaxVelocity,WindowSize,TTL}}),
  {next_state, paint, State#guiStateM_state{demi = DemiZone}};

paint(cast, _, State = #guiStateM_state{}) ->
  {next_state, waiting, State}.

%When a user clicks a button we check which one he clicked and updates the StateMachine about the Click event
handle_click(#wx{obj = Obj, userData = #{text := T, env := Env}},_Event) ->
  wx:set_env(Env),
  wxStaticText:setLabel(T, "Running.."),
  Label = wxButton:getLabel(Obj),
  if (Label == "Start") -> gen_statem:cast({global, ?SERVER}, paintnow);
    true -> gen_statem:cast({global, ?SERVER}, {sendnewStats,Env})
  end.

handle_sync_event(#wx{event=#wxErase{}}, _, _) -> ok.


%do refresh been callled every RefreshTimer interval, creates a BufferedPainDC to create a bitmap of the next frame befor
%drawing, to eliminate flickering.
do_refresh(#guiStateM_state{numOfProcesses = NumOfProcess, liveStats = LiveStats, frame = F,
  canvas = C,nodesList = NodesList, demi = DemiZone},EtsList)->

  %Create a PainDC with a bitmap buffer within. this will eliminate flickering.
  DC = wxBufferedPaintDC:new(C),
  wxDC:clear(DC),

  %update the Number of Processes label -
  wxStaticText:setLabel(LiveStats, ["Number of processes: ",integer_to_list(NumOfProcess)]),

  %first draw lines of the field
  wxDC:setBrush(DC, ?wxTRANSPARENT_BRUSH),
  wxDC:setPen(DC, wxPen:new(?wxBLACK, [{width, 2}])),
  wxDC:drawRectangle(DC,{0,0},{?Width,?Width}),
  wxDC:drawLine(DC,{?Width div 2,0},{?Width div 2,?Width}),
  wxDC:drawLine(DC,{0,?Width div 2},{?Width,?Width div 2}),

  % draw lines of the demilitarized zone
  Demi = DemiZone div 4,
  wxDC:setPen(DC, wxPen:new(?wxLIGHT_GREY, [{width, 2}])),
  wxDC:drawLine(DC,{?Width div 2 -Demi,0},{?Width div 2 - Demi,?Width}),
  wxDC:drawLine(DC,{?Width div 2 +Demi,0},{?Width div 2 + Demi,?Width}),
  wxDC:drawLine(DC,{0,?Width div 2 + Demi},{?Width,?Width div 2 + Demi}),
  wxDC:drawLine(DC,{0,?Width div 2 - Demi},{?Width,?Width div 2 - Demi}),


  %draw all robins from etsrobins, each robin in the color matches by its node
  wxDC:setPen(DC, wxPen:new(?wxRED, [{width, 2}])),
  [ paintCirclesinColors(DC,NodesList,Node,X div 4,Y div 4 ) || {{_Pid,Node},{X,Y}}<- EtsList],
  wxDC:setPen(DC, wxPen:new(?wxRED, [{width, 2}])),

  %draw the red lines of messages sent from etsMSGs
  [wxDC:drawLine(DC,{XFrom div 4,YFrom div 4}, {XTo div 4,YTo div 4})||{{XFrom,YFrom}, {XTo,YTo}}<-getMsgPids()],

  %New frame is ready to show
  wxBufferedPaintDC:destroy(DC),
  wxWindow:show(F).



%%
getMsgPids() ->getMsgPids([],ets:first(etsMsgs)).
getMsgPids(L,'$end_of_table') ->L;%end  return List Of XYFrom, XYTo
%For each msg in etsMsgs, take the timer down by one and if its 0 delete the msg
%XYRobins is a list of {XYFrom location, XYTo location}
getMsgPids(XYRobinsLocations,{From,To}) ->
  [{_FromTo, Timer}] = ets:lookup(etsMsgs,{From,To}),
  Key = {From,To},
  Next = ets:next(etsMsgs,Key),
  try
  if Timer == 0 ->
    ets:delete(etsMsgs,Key),
    getMsgPids(XYRobinsLocations,Next);
    true ->
      [{{_Pid,_Node}, {XFrom,YFrom}}] = ets:lookup(etsRobins,From),
      [{{_Pid2,_Node2}, {XTo,YTo}}] = ets:lookup(etsRobins,To),
        ets:delete(etsMsgs,Key),
        ets:insert(etsMsgs,{Key,(Timer-1)}),

      getMsgPids(XYRobinsLocations++[{{XFrom,YFrom},{XTo,YTo}}],Next)
      end
  catch
    _:_->
      ets:delete(etsMsgs,Key),
      getMsgPids(XYRobinsLocations,Next)
  end.

paintCirclesinColors(DC,NodesList,Node,X,Y) ->%Set a different color for each node.
  wxDC:setPen(DC, wxPen:new(getColor(Node,NodesList), [{width, 2}])) ,wxDC:drawCircle(DC, {X,Y}, 3).

getColor(Node,[{Node,Color}|_])->Color;
getColor(Node,[_|NodesList])->getColor(Node,NodesList).


%% @private
%% @doc If callback_mode is handle_event_function, then whenever a
%% gen_statem receives an event from call/2, cast/2, or as a normal
%% process message, this function is called.
handle_event(_EventType, _EventContent, _StateName, State = #guiStateM_state{}) ->
  NextStateName = the_next_state_name,
  {next_state, NextStateName, State}.


%% @private
%% @doc This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
terminate(_Reason, _StateName, _State = #guiStateM_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
code_change(_OldVsn, StateName, State = #guiStateM_state{}, _Extra) ->
  {ok, StateName, State}.
