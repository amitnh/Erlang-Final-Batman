%%%-------------------------------------------------------------------
%%% @author amit and kapelnik
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Jul 2020 11:33
%%%-------------------------------------------------------------------
-module(gui).
-author("kapelnik").
%% API
-export([init/0,handle_click/2]).
-include_lib("wx/include/wx.hrl").
-define(Height, 1000).
-define(Width, 1000).



init() ->do_init().%%wx:batch(fun() -> do_init() end).

-record(state,
        {
          frame,
          env,
          panel,
          text,
          canvas,
          bitmap,
          overlay,
          pos
        }).
do_init() ->    %%F=frame P=panel C=canvas
  Env = wx:new(),     %%create a wx environment
  F = wxFrame:new(wx:null(), -1, "B.A.T.M.A.N Display", [{size, {?Width,?Height}}]),  %Creates the main frame for the gui
  P = wxPanel:new(F, [{size, {?Width,?Height-100}}]), % a panel we will split with sizers
  C = wxPanel:new(P, [{style, ?wxFULL_REPAINT_ON_RESIZE}]), %the main canvas to print on the points
  MainSizer = wxBoxSizer:new(?wxVERTICAL),      %main sizer for alignment within the panel
  Sizer = wxStaticBoxSizer:new(?wxVERTICAL, P, [{label, "Batman bounderies"}]), %inside frame for batman protocol display
  T = wxStaticText:new(P, -1, "Click to start",[]),

  %create a botton to initiate batman protocol and connect it to its event handler
  B = wxButton:new(P, 0, [{label, "Start"}, {size, {150, 50}}]),
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

  initcanvas(C),
  wxFrame:show(F),
  mainLoop(#state{canvas = C,text = T, env = Env}).

%%c(gui). gui:init().
%%c(gui). gui:init().
%%c(gui). gui:init().
%%c(gui). gui:init().
%%c(gui). gui:init().

handle_click(S=#wx{obj = _, userData = #{canvas := C,text := T, env := Env}},_Event) ->
  wx:set_env(Env),
  wxStaticText:setLabel(T, "Running..").

mainLoop(S=#state{ canvas= C ,text = T, env = Env}) ->

  DC = wxPaintDC:new(C),
  X=rand:uniform(10),
  wxDC:clear(DC),
  wxDC:setBrush(DC, ?wxTRANSPARENT_BRUSH),
  wxDC:setPen(DC, wxPen:new(?wxBLACK, [{width, 2}])),
  wxDC:drawCircle(DC, {200+X, 200-X}, 3),
  wxDC:drawCircle(DC, {1+X, 2-X}, 3),
  wxPaintDC:destroy(DC),
  receive
    after 16->mainLoop(S)
  end.



initcanvas(C) ->
  %%
  DC = wxPaintDC:new(C),
  Image = wxImage:new("image.jpg"),
  Image2 = wxImage:scale(Image, wxImage:getWidth(Image) div 3,
  wxImage:getHeight(Image) div 3),
  Bmp = wxBitmap:new(Image2),
  wxImage:destroy(Image),
  wxImage:destroy(Image2),

  wxDC:clear(DC),
  wxBitmap:destroy(Bmp),
  wxPaintDC:destroy(DC).




%%
%%  wxDC:clear(DC),
%%  lists:foreach(fun({X,Y}=Pos) ->
%%    wxDC:setBrush(DC, ?wxTRANSPARENT_BRUSH),
%%    wxDC:setPen(DC, wxPen:new(?wxBLACK, [{width, 2}])),
%%    case X rem 6 of
%%      0 -> wxDC:drawBitmap(DC, Bmp, Pos);
%%      1 -> wxDC:setBrush(DC, ?wxRED_BRUSH),
%%        wxDC:drawRectangle(DC, Pos, {20,20});
%%      2 -> wxDC:setBrush(DC, ?wxBLUE_BRUSH),
%%        wxDC:drawCircle(DC, {X+10, Y+10}, 15);
%%      3 -> wxDC:setPen(DC, wxPen:new({200,200,0,255}, [{width, 4}])),
%%        wxDC:drawLine(DC, Pos, get_pos(W,H));
%%      4 -> wxDC:setBrush(DC, ?wxGREEN_BRUSH),
%%        wxDC:drawEllipse(DC, Pos, {60,20});
%%      _ -> wxDC:drawLabel(DC, "Erlang /", {X,Y,60,20}),
%%        wxDC:drawRotatedText(DC, "OTP", {X+60,Y}, 340.0)
%%    end
%%                end, Positions)