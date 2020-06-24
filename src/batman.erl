%%%-------------------------------------------------------------------
%%% @author amit
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(batman).
-author("amit").
-include_lib("wx/include/wx.hrl").

%% API
-behaviour(wx_object).
-export([test/0]).


test() -> wx:new(),
  F=wxFrame:new(wx:null(),?wxID_ANY,"Field of nodebarries"),
  wxStaticText:new(F, ?wxID_ANY,"cos emek",[{style, ?wxALIGN_RIGHT}]),
%%  Counter = wxStaticText:new(F, ?wxID_ANY,integer_to_list(10)),
%%  wxFrame:show(F),
%%  countdown(9,Counter).
%%
%%countdown(0,_) -> ok;
%%countdown(S,C) -> timer:sleep(400),
%%  wxStaticText:setLabel(C,integer_to_list(S)),
%%  countdown(S-1,C).


%%  M= wxMessageDialog:new(wx:null(),"jump to me", [{style, ?wxYES_NO},{caption,"amit nagar halevy"}]),
%%
%%
%%  wxMessageDialog:showModal(M).
%%test() -> WX =wx:new(),
%%          Game_Images = loading_Images_to_tupleList(),
%%          BG= getValueByKey(Game_Images,background),
%%          SizeOfBG={wxImage:getWidth(BG),wxImage:getHeight(BG)},
%%          Frame = wxFrame:new(WX,-1,"Warcrasd", [{size,SizeOfBG}]),
%%          Panel = wxPanel:new(Frame),
%%          menubar:setmenu(Frame),
%%          wxFrame:show(Frame),
%%          loop(Frame,Panel,Game_Images).
%%
%%loop(Frame,Panel,Game_Images) ->
%%    menubar:menu_receiver(Frame,self()),
%%    receive
%%        stop -> wxFrame:destroy(Frame),stop()
%%    after 40 ->
%%        drawing(Frame,Panel,Game_Images),loop(Frame,Panel,Game_Images)
%%    end.
%%
%%
%%drawing(Frame,Panel,Game_Images) ->
%%  ClientDC = wxClientDC:new(Panel),
%%  DC=wxBufferDC:new(ClientDC),
%%  BG = wxBitmap:new(getValueByKey(Game_Images,background)),
%%  wxDC:drawBitmap(DC,BG,{0,0}),
%%  Lifes = 7055, Gold = 4575, Mode = "Defence",
%%  Font = wxFont(12, ?wxFONTFAMILY_DECORATIVE, ?wxFONTSTYLE_NORMAL, ?wxFONTWEIGHT_BOLD,[]),
%%  wxDC:setFont(DC,Font),
%%  wxDC:setTextForeground(DC,?wxRED),
%%  wxDC:drawText(DC,"Lifes: "++integer_to_list(Lifes)++"   Gold: "++integer_to_list(Gold)++" Mode: "++Mode,{45,890}),
%%  wxDC:setTextForeground(DC,?wxGREEN),
%%  wxDC:drawText(DC,"Lifes: "++integer_to_list(Lifes)++"   Gold: "++integer_to_list(Gold)++" Mode: "++Mode,{1180,10}),
%%  wxDC:setTextForeground(DC,?wxBLACK),
%%  wxDC:drawText(DC,"Player notes ",{600,890}).