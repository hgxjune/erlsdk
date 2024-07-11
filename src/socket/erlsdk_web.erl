%%------------------------------------------------------------------------------
% @doc erlsdk_web module
% @author hgx <hgx@live.cn>
% @copyright 2024 hgx, All rights reserved.
% @since 2024-07-11 
%%------------------------------------------------------------------------------
-module(erlsdk_web).
-include("debug.hrl").

-export([reply_404/1]).
-export([reply_404/2]).
-export([reply_200/2]).

%% public functions
reply_404(Req0) ->
    Req1 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<"*">>, Req0),
    cowboy_req:reply(404, #{}, <<>>, Req1).

reply_404(Req0, Data) ->
    Req1 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<"*">>, Req0),
    cowboy_req:reply(404, #{<<"content-type">> => <<"application/json; charset=utf-8">>}, Data, Req1).

reply_200(Req0, Data) ->
    Req1 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<"*">>, Req0),
    cowboy_req:reply(200, #{<<"content-type">> => <<"application/json; charset=utf-8">>}, Data, Req1).




%% internal functions

