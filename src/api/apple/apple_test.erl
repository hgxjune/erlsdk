%%------------------------------------------------------------------------------
% @doc apple_test module
% @author hgx <hgx@live.cn>
% @copyright 2024 hgx, All rights reserved.
% @since 2024-07-13 
%%------------------------------------------------------------------------------
-module(apple_test).

-export([notifications/1]).

%% public functions
% @doc https://developer.apple.com/documentation/appstoreserverapi/request_a_test_notification

t1() -> t("https://api.storekit-sandbox.itunes.apple.com/inApps/v1/notifications/test").
t2() -> t("https://api.storekit.itunes.apple.com/inApps/v1/notifications/test").


t(Url) ->
    JWT         = request_jwt(),
    HttpBody    = jsx:encode(#{}),
    ContentType = "application/json",
    Author      = <<"Bearer ", JWT/binary>>,
    HttpHeaders = [ {"authorization", Author} ],
    Request     = {Url, HttpHeaders, ContentType, HttpBody},
    HttpOpts    = [{timeout, 5000}],
    Options     = [],

    case httpc:request(post, Request, HttpOpts, Options) of
        {ok, {_S, _H, RBody}} ->
            ?debug("notifications/test request, StatusLine: ~p, HttpHeader: ~p, Body: ~p, HttpBody: ~p", [_S, _H, RBody, HttpBody]),
            ok;
        Err->
            ?WARNING("notifications/test request err, HttpBody: ~p, Err: ~p", [HttpBody, Err]),
            ok
    end.



%% internal functions
%%------------------------------------------------------------------------------
% @doc https://developer.apple.com/documentation/appstoreserverapi/generating_json_web_tokens_for_api_requests
request_jwt() ->
    Headers = #{ alg => <<"ES256">>
               , kid => <<"5U233C98Q5">>
               , typ => <<"JWT">>
               },
    TS      = utils:timestamp(),
    Payload = #{ iss => <<"63a4fe73-41e0-401b-ab0e-6bd5a86c2781">>
               , iat => TS
               , exp => TS + 3600  % 此处时间设置在 1 小时以内，否则会 401
               , aud => <<"appstoreconnect-v1">>
               , bid => <<"com.ios.zyppds">>
               },
    H       = base64:encode( jsx:encode(Headers), #{padding => false, mode => urlsafe} ),
    P       = base64:encode( jsx:encode(Payload), #{padding => false, mode => urlsafe} ),

    Content = <<H/binary, ".", P/binary>>,
    PriKey  = <<"-----BEGIN PRIVATE KEY-----\nMIGTAgEAMBMGByqGSM49AgEGCCqGSM49AwEHBHkwdwIBAQQgUKwpbCv16GbSMJELiYpgRTYdJbqpDi92fvcMF2WZY1agCgYIKoZIzj0DAQehRANCAASfOzyvl39xl5KKRgY8Yk+LJq9jTiI5r030JaporSnOGmxfz/D65N0mm1rrF+kWKEwNVrE0VDcs6MTnevBnsZgB\n-----END PRIVATE KEY-----">>,

    [Entry] = public_key:pem_decode(PriKey),
    RSAKey  = public_key:pem_entry_decode(Entry),
    SignBin = public_key:sign(Content, sha256, RSAKey),
    SignRaw = raw(SignBin),
    SignB64 = base64:encode(SignRaw, #{padding => false, mode => urlsafe}),

    Secret  = <<Content/binary, ".", SignB64/binary>>,
    Secret.
