%%------------------------------------------------------------------------------
% @doc route_root module
% @author hgx <hgx@live.cn>
% @copyright 2024 hgx, All rights reserved.
% @since 2024-07-11 
%%------------------------------------------------------------------------------
-module(route_root).
-include("debug.hrl").

-export([init/2]).

init(Req0, Opts) ->
    try
        case maps:get(method, Req0) of
            <<"GET">> -> request_get(Req0, Opts);
            <<"POST">> -> request_post(Req0, Opts);
            _ -> {ok, erlsdk_web:reply_404(Req0), Opts}
        end
    catch
        Class:Reason:Stack ->
            ?debug("web critical error, Class:~p, Reason:~p, Stack:~p", [Class, Reason, Stack]),
            ?debug("Req0: ~p", [Req0]),
            Req = erlsdk_web:reply_404(Req0),
            {ok, Req, Opts}
    end.


%%%-------------------------------------------------------------------
%% get
request_get(Req0, Opts) ->
    Path = cowboy_req:path(Req0),
    Qs   = cowboy_req:qs(Req0),
    Req  = request_get(Req0, Path, Qs),
    {ok, Req, Opts}.

%%
request_get(Req0, Path, Body) ->
    ?debug("get > path: ~p, qs: ~p", [Path, Body]),
    erlsdk_web:reply_404(Req0).


%%%-------------------------------------------------------------------
%% post
request_post(Req0, Opts) ->
    Path = cowboy_req:path(Req0),
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    Req  = request_post(Req1, Path, Body),
    {ok, Req, Opts}.


request_post(Req0, _, <<>>) -> erlsdk_web:reply_404(Req0, <<"{\"code\":99,\"message\":\"err\"}">>);


request_post(Req0, Path, Body0) ->
    ?debug("post > path: ~p, body: ~p", [Path, Body0]),
    erlsdk_web:reply_404(Req0).