%%------------------------------------------------------------------------------
% @doc erlsdk module
% @author hgx <hgx@live.cn>
% @copyright 2024 hgx, All rights reserved.
% @since 2024-07-11 
%%------------------------------------------------------------------------------
-module(erlsdk).

-export([start/0, stop/0]).

%% public functions
start() ->
    {ok, _} = application:ensure_all_started(erlsdk),
    ok.

stop() ->
    application:stop(erlsdk),
    init:stop(),
    ok.



%% internal functions

