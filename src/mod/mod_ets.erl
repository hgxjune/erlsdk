%%------------------------------------------------------------------------------
% @doc mod_ets module
% @author hgx <hgx@live.cn>
% @copyright 2024 hgx, All rights reserved.
% @since 2024-07-11 
%%------------------------------------------------------------------------------
-module(mod_ets).
-behaviour(gen_server).

-include("debug.hrl").
-include("ets.hrl").

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start_link/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% ====================================================================
%% External functions
%% ====================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% ====================================================================
init([]) ->
    init_ets(),
    {ok, []}.


handle_call(_Info, _From, State) ->
    ?debug(_Info),
    {reply, ok, State}.


handle_cast(_Info, State) ->
    ?debug(_Info),
    {noreply, State}.


handle_info(_Info, State) ->
    ?debug(_Info),
    {noreply, State}.


terminate(_Reason, _Status) ->
    ok.


code_change(_oldvsn, Status, _extra) ->
    {ok, Status}.


%% ====================================================================
%% Local functions
%% ====================================================================
init_ets() ->
    ets:new(?ETS_APPLE,     [{keypos, 1},   named_table, public, set]),
    ok.

