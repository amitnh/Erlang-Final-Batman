%%%-------------------------------------------------------------------
%%% @author kapelnik
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Aug 2020 15:07
%%%-------------------------------------------------------------------
-module(guiStateM).
-author("kapelnik").

-behaviour(gen_statem).

%% API
-export([start_link/0]).

%% gen_statem callbacks
-export([init/1, format_status/2, state_name/3, handle_event/4, terminate/3,
  code_change/4, callback_mode/0, waiting/3, paint/3]).
-include_lib("wx/include/wx.hrl").
-define(Height, 1000).
-define(Width, 1000).
-define(RefreshRate, 20).
-define(SERVER, ?MODULE).
-record(guiStateM_state,
{
  frame,
  env,
  panel,
  text,
  canvas
}).
%%-record(guiStateM_state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
start_link() ->
  gen_statem:start_link({global, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%% @private
%% @doc Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
init([]) ->
  % {Pid,Node} -> {X,Y}, {{<0.112.0>,tal@ubuntu},X,Y}
%%  ets:new(etsRobins,[set,public,named_table]),
%%  ets:insert(etsRobins, [{pid1,{200,60}}, {pid2,{300,20}}, {pid3,{500,1000}}, {pid4,{40,800}}]),

  Env = wx:new(),     %%create a wx environment
  F = wxFrame:new(wx:null(), -1, "B.A.T.M.A.N Display", [{size, {?Width+100,?Height}}]),  %Creates the main frame for the gui
  P = wxPanel:new(F, [{size, {?Width,?Height-100}}]), % a panel we will split with sizers
  C = wxPanel:new(P, [{style, ?wxFULL_REPAINT_ON_RESIZE},{size, {?Width,?Height-100}}]), %the main canvas to print on the points
  MainSizer = wxBoxSizer:new(?wxVERTICAL),      %main sizer for alignment within the panel
  Sizer = wxStaticBoxSizer:new(?wxVERTICAL, P, [{label, "Batman bounderies"}]), %inside frame for batman protocol display
  T = wxStaticText:new(P, -1, "Click to start",[]),

  %create a botton to initiate batman protocol and connect it to its event handler
  B = wxButton:new(P, 0, [{label, "Start"}, {size, {150, 50}}]),
  State = #guiStateM_state{canvas = C,text = T, env = Env},
  wxButton:connect(B, command_button_clicked, [{callback, fun handle_click/2}, {userData, #{canvas => C,text => T, env => wx:get_env()}}]),
  %Connect the Panel to paint event
  wxPanel:connect(P, paint, [callback]),
  wxSizer:add(Sizer, B, [{border, 5}, {flag, ?wxALL}]),
  wxSizer:addSpacer(Sizer, 5),
  wxSizer:add(Sizer, T, [{border, 5}, {flag, ?wxALL}]),
  wxSizer:addSpacer(Sizer, 5),
  wxSizer:add(Sizer, C, [{flag, ?wxEXPAND}, {proportion, 1}]),
  wxSizer:add(MainSizer, Sizer, [{flag, ?wxEXPAND}, {proportion, 1}]),
  wxPanel:setSizer(P, MainSizer),
  wxSizer:layout(MainSizer),

  %Frame is ready for display

%%  initcanvas(C),
  wxFrame:show(F),
  {ok, waiting, #guiStateM_state{env = Env, canvas = C,frame = F,panel = P,text = T}}.



%%c(guiStateM). guiStateM:start_link().
%%c(guiStateM). guiStateM:start_link().
%%c(guiStateM). guiStateM:start_link().
%%c(guiStateM). guiStateM:start_link().
%%c(guiStateM). guiStateM:start_link().



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

%%c(guiStateM). guiStateM:start_link().
%%c(guiStateM). guiStateM:start_link().
%%c(guiStateM). guiStateM:start_link().
state_name(star, _EventContent, State = #guiStateM_state{}) ->
  NextStateName = next_state,
  {next_state, NextStateName, State}.

waiting(cast,paintnow,State = #guiStateM_state{}) ->
  spawn(fun()-> timer() end),
  {next_state, paint, State};

waiting(cast, _, State = #guiStateM_state{}) ->
  {next_state, waiting, State}.

paint(cast,refresh,State = #guiStateM_state{canvas = C}) ->

  do_refresh(C),
  {next_state, paint, State};

paint(cast, _, State = #guiStateM_state{}) ->
  {next_state, paint, State}.


handle_click(#wx{obj = _, userData = #{text := T, env := Env}},_Event) ->
  wx:set_env(Env),
  wxStaticText:setLabel(T, "Running.."),
  gen_statem:cast({global, ?SERVER}, paintnow).

timer()->
  receive
    after ?RefreshRate -> gen_statem:cast({global, ?SERVER}, refresh)
  end,
  timer().

do_refresh(C)->
  EtsList = ets:tab2list(etsRobins),
  DC = wxPaintDC:new(C),

  wxDC:setBrush(DC, ?wxTRANSPARENT_BRUSH),
  wxDC:setPen(DC, wxPen:new(?wxBLACK, [{width, 2}])),
  wxDC:clear(DC),
  wxDC:drawRectangle(DC,{0,0},{1000,1000}),
  wxDC:drawLine(DC,{500,0},{500,1000}),
  wxDC:drawLine(DC,{0,500},{1000,500}),
  wxDC:setPen(DC, wxPen:new(?wxRED, [{width, 2}])),
  [wxDC:drawCircle(DC, {X div 2,Y div 2}, 3) || {_,{X,Y}}<- EtsList],
  wxPaintDC:destroy(DC).


%%initcanvas(C) ->ok.
%%%%  DC = wxPaintDC:new(C),
%%%%  Image = wxImage:new("image.jpg"),
%%%%%%  Image2 = wxImage:scale(Image, wxImage:getWidth(Image) div 3,
%%%%%%  wxImage:getHeight(Image) div 3),
%%%%  Image2 = wxImage:scale(Image, ?Width,?Height-100),
%%%%  Bmp = wxBitmap:new(Image2),
%%%%  wxImage:destroy(Image),
%%%%  wxImage:destroy(Image2),
%%%%
%%%%  wxDC:clear(DC),
%%%%  wxBitmap:destroy(Bmp),
%%%%  wxPaintDC:destroy(DC).



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

%%%===================================================================
%%% Internal functions
%%%===================================================================
