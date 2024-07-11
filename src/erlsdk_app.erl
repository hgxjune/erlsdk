%%%-------------------------------------------------------------------
%% @doc erlsdk public API
%% @end
%%%-------------------------------------------------------------------

-module(erlsdk_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    inets:start(),
    erlsdk_socket:start(),
    erlsdk_sup:start_link().

stop(_State) ->
    erlsdk_socket:stop(),
    ok.

%% internal functions
