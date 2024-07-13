%%------------------------------------------------------------------------------
% @doc apple_test module
% @author hgx <hgx@live.cn>
% @copyright 2024 hgx, All rights reserved.
% @since 2024-07-13 
%%------------------------------------------------------------------------------
-module(apple_test).
-include("debug.hrl").

-export([notifications/1]).

%% public functions
% @doc https://developer.apple.com/documentation/appstoreserverapi/request_a_test_notification
notifications(1)        -> notifications(sandbox);
notifications(2)        -> notifications(produce);
notifications(sandbox)  -> do_notifications( apple_config:url_test_sandbox() );
notifications(produce)  -> do_notifications( apple_config:url_test_produce() );
notifications(_)        ->
    io:format("notifications:\n"
              "  - 1         see sandbox.\n"
              "  - sandbox   Request a Test Notification to Sandbox.\n"
              "  - 2         see produce.\n"
              "  - produce   Request a Test Notification to Produce.\n"
          ),
    ok.

do_notifications(Url) ->
    HttpBody    = jsx:encode(#{}),
    ContentType = "application/json",

    JWT         = purchase_token(),
    Author      = <<"Bearer ", JWT/binary>>,
    HttpHeaders = [ {"authorization", Author} ],

    Request     = {Url, HttpHeaders, ContentType, HttpBody},
    HttpOpts    = [{timeout, 5000}],
    Options     = [],

    case httpc:request(post, Request, HttpOpts, Options) of
        {ok, {_S, _H, RBody}} ->
            ?debug("notifications/test request, StatusLine: ~p, Body: ~p, HttpBody: ~p", [_S, RBody, HttpBody]),
            ok;
        Err->
            ?debug("notifications/test request err, HttpBody: ~p, Err: ~p", [HttpBody, Err]),
            ok
    end.



%% internal functions
%%------------------------------------------------------------------------------
% @doc https://developer.apple.com/documentation/appstoreserverapi/generating_json_web_tokens_for_api_requests
purchase_token() ->
    Headers = #{ alg => <<"ES256">>
               , kid => apple_config:purchase_key_id()
               , typ => <<"JWT">>
               },
    TS      = utils:timestamp(),
    Payload = #{ iss => apple_config:issuer_id()
               , iat => TS
               , exp => TS + 1800  % 此处时间设置在 1 小时以内，否则会 401
               , aud => <<"appstoreconnect-v1">>
               , bid => apple_config:bundle_id()
               },
    PriKey  = apple_config:purchase_key(),
    apple_tool:jwt(Headers, Payload, PriKey).


