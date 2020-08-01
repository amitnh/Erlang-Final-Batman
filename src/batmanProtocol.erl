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
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

%%%===================================================================
-record(batmanProtocol_state, {known}).
%% known is a map of known Robins in the system
%% each Robin contains map of neighbors Pid that he received messages from
%% each neighbor contain a sorted list of Relevant (in-window) sequence numbers for the relevant Robin
%%%===================================================================
-define(windowSize, 128). %% Define the size of the window. only sequance numbers in the window will be counted and saved
-define(TTL, 40). %% Time To Live, travel distance of a message


%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #batmanProtocol_state{}} | {ok, State :: #batmanProtocol_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
%%  spawn_link(moveSimulator,start_link(),[]),  %creates moveSimulator and link it to this process TODO: remove comment
  {ok, #batmanProtocol_state{known = maps:new()}}. %return a new empty map of known Robins

ogmLoop()-> receive
              after 1000-> sendOGM %TODO sendOGM function -> call get negibors from moveSimulator and sends tem the message
            end.
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

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #batmanProtocol_state{},
    Extra :: term()) ->
  {ok, NewState :: #batmanProtocol_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #batmanProtocol_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
