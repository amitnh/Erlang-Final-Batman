%%%-------------------------------------------------------------------
%%% @author kapelnik
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
          bitmap,
          overlay,
          pos
        }).
do_init() ->
  wx:new(),
  F = wxFrame:new(wx:null(), -1, "B.A.T.M.A.N Display", [{size, {?Width,?Height}}]),
  P = wxPanel:new(F, [{size, {?Width,?Height-100}}, {pos,{500, 500}}]),

  MainSizer = wxBoxSizer:new(?wxVERTICAL),
  Sizer = wxStaticBoxSizer:new(?wxVERTICAL, P, [{label, "Batman bounderies"}]),
  T = wxStaticText:new(P, -1, "Displaying..",[{pos,{0, 50}}]),
  B = wxButton:new(P, 0, [{label, "Start"}, {size, {150, 50}}]),
  wxButton:connect(B, command_button_clicked, [{callback, fun handle_click/2}, {userData, #{text => T, env => wx:get_env()}}]),

  wxSizer:add(Sizer, B, [{border, 5}, {flag, ?wxALL}]),
  wxSizer:addSpacer(Sizer, 5),
  wxSizer:add(Sizer, P, [{flag, ?wxEXPAND}, {proportion, 1}]),
  wxSizer:add(MainSizer, Sizer, [{flag, ?wxEXPAND}, {proportion, 1}]),
  wxPanel:setSizer(P, MainSizer),
  wxSizer:layout(MainSizer),
%%  {W,H} = wxPanel:getSize(P),
%%  Bitmap = wxBitmap:new(erlang:max(W,30),erlang:max(30,H)),
  wxFrame:show(F),

  mainLoop(#state{frame = F, env = wx:get_env(),panel = P}).

handle_click(#wx{obj = _, userData = #{text := T, env := Env}},_Event) ->
  wx:set_env(Env),
  wxStaticText:setLabel(T, "clicked").

mainLoop(#state{env = Env, frame = F , panel = P}) -> okay.





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