%%------------------------------------------------------------------------------
% @doc apple_user module
% @author hgx <hgx@live.cn>
% @copyright 2024 hgx, All rights reserved.
% @since 2024-07-11 
%%------------------------------------------------------------------------------
-module(apple_user).
-include("debug.hrl").
-include_lib("public_key/include/public_key.hrl").

-export([login/1]).

%% public functions
% 两种验证方式
% 1.  对客户端的 identityToken 进行 JWT 算法验证
% 2.  基于授权码的验证 OAuth 2.0
login(BodyIn) ->
    DataIn          = jsx:decode(BodyIn),
    AuthorCode      = maps:get(<<"authorizationCode">>, DataIn, undefined),
    UserId          = maps:get(<<"userId">>,            DataIn, undefined),
    IdentityToken   = maps:get(<<"identityToken">>,     DataIn, undefined),

    % 1.  对客户端的 identityToken 进行 JWT 算法验证
    VerifyClient    = validate_client_token(IdentityToken),
    ?debug("authorizationCode   : ~p", [AuthorCode]),
    ?debug("userId              : ~p", [UserId]),
    ?debug("identityToken       : ~p", [IdentityToken]),
    ?debug("VerifyClient        : ~p", [VerifyClient]),

    [_, D1, _]      = binary:split(IdentityToken, <<".">>, [global]),
    D1Str           = base64:decode(D1, #{padding => false, mode => urlsafe}),
    D1Map           = jsx:decode(D1Str),
    ?debug("-- identityToken data begin"),
    ?debug("iss                 : ~p", [maps:get(<<"iss">>,             D1Map, undefined)]),
    ?debug("aud                 : ~p", [maps:get(<<"aud">>,             D1Map, undefined)]),
    ?debug("exp                 : ~p", [maps:get(<<"exp">>,             D1Map, undefined)]),
    ?debug("iat                 : ~p", [maps:get(<<"iat">>,             D1Map, undefined)]),
    ?debug("sub                 : ~p", [maps:get(<<"sub">>,             D1Map, undefined)]),
    ?debug("c_hash              : ~p", [maps:get(<<"c_hash">>,          D1Map, undefined)]),
    ?debug("email               : ~p", [maps:get(<<"email">>,           D1Map, undefined)]),
    ?debug("email_verified      : ~p", [maps:get(<<"email_verified">>,  D1Map, undefined)]),
    ?debug("is_private_email    : ~p", [maps:get(<<"is_private_email">>,D1Map, undefined)]),
    ?debug("auth_time           : ~p", [maps:get(<<"auth_time">>,       D1Map, undefined)]),
    ?debug("nonce_supported     : ~p", [maps:get(<<"nonce_supported">>, D1Map, undefined)]),
    ?debug("-- identityToken data end"),

    % 2.  基于授权码的验证 OAuth 2.0
    RequestMap      = generate_and_validate_tokens(AuthorCode),
    AccessToken     = maps:get(<<"access_token">>,      RequestMap, undefined),
    TokenType       = maps:get(<<"token_type">>,        RequestMap, undefined),
    ExpiresIn       = maps:get(<<"expires_in">>,        RequestMap, undefined),
    RefreshToken    = maps:get(<<"refresh_token">>,     RequestMap, undefined),
    IdToken         = maps:get(<<"id_token">>,          RequestMap, undefined),
    
    ?debug("access_token        : ~p", [AccessToken]),
    ?debug("token_type          : ~p", [TokenType]),
    ?debug("expires_in          : ~p", [ExpiresIn]),
    ?debug("refresh_token       : ~p", [RefreshToken]),
    ?debug("id_token            : ~p", [IdToken]),

    [_, D2, _]      = binary:split(identityToken, <<".">>, [global]),
    D2Str           = base64:decode(D2, #{padding => false, mode => urlsafe}),
    D2Map           = jsx:decode(D2Str),
    ?debug("-- id_token data begin"),
    ?debug("iss                 : ~p", [maps:get(<<"iss">>,             D2Map, undefined)]),
    ?debug("aud                 : ~p", [maps:get(<<"aud">>,             D2Map, undefined)]),
    ?debug("exp                 : ~p", [maps:get(<<"exp">>,             D2Map, undefined)]),
    ?debug("iat                 : ~p", [maps:get(<<"iat">>,             D2Map, undefined)]),
    ?debug("sub                 : ~p", [maps:get(<<"sub">>,             D2Map, undefined)]),
    ?debug("at_hash             : ~p", [maps:get(<<"at_hash">>,         D2Map, undefined)]),
    ?debug("email               : ~p", [maps:get(<<"email">>,           D2Map, undefined)]),
    ?debug("email_verified      : ~p", [maps:get(<<"email_verified">>,  D2Map, undefined)]),
    ?debug("is_private_email    : ~p", [maps:get(<<"is_private_email">>,D2Map, undefined)]),
    ?debug("auth_time           : ~p", [maps:get(<<"auth_time">>,       D2Map, undefined)]),
    ?debug("nonce_supported     : ~p", [maps:get(<<"nonce_supported">>, D2Map, undefined)]),
    ?debug("-- id_token data end"),

    jsx:encode( #{ errcode  => 0
                 , errmsg   => <<"SUCCESS">>
                 , userid   => UserId
                 } ).

%% internal functions
%%------------------------------------------------------------------------------
% 1.  对客户端的 identityToken 进行 JWT 算法验证
validate_client_token(Token) ->
    [H, D, S]   = binary:split(Token, <<".">>, [global]),
    Content     = <<H/binary, ".", D/binary>>,
    DigestType  = sha256,
    Signature   = base64:decode(S, #{padding => false, mode => urlsafe}),

    Header      = base64:decode(H, #{padding => false, mode => urlsafe}),
    HeaderMap   = jsx:decode(Header),
    KeyId       = maps:get(<<"kid">>, HeaderMap, undefined),
    PubKey      = auth_keys(KeyId),

    public_key:verify(Content, DigestType, Signature, PubKey).


%%------------------------------------------------------------------------------
% 2.  基于授权码的验证 OAuth 2.0
% Generate and validate tokens
% @doc https://developer.apple.com/documentation/sign_in_with_apple/generate_and_validate_tokens

% URL: POST https://appleid.apple.com/auth/token
% HTTP Body: Content-Type: application/x-www-form-urlencoded
% Parts
% Name            Type        PS
% client_id       string      (Required) The identifier (App ID or Services ID) for your app.
% client_secret   string      (Required) A secret JSON Web Token, generated by the developer, that uses the Sign in with Apple private key associated with your developer account.
% code            string      The authorization code received in an authorization response sent to your app.
% grant_type      string      (Required) The grant type determines how the client app interacts with the validation server. authorization_code or refresh_token
% refresh_token   string      The refresh token received from the validation server during an authorization request.
% redirect_uri    string      The destination URI provided in the authorization request when authorizing a user with your app, if applicable.

% TokenResponse
% Name            Type        PS
% access_token    string      A token used to access allowed data
% expires_in      number      The amount of time, in seconds, before the access token expires.
% id_token        string      A JSON Web Token (JWT) that contains the user’s identity information.
% refresh_token   string      The refresh token used to regenerate new access tokens when validating an authorization code.
% token_type      string      The type of access token, which is always bearer.
generate_and_validate_tokens(AuthorCode) ->
    DataMaps    = [ {<<"client_id">>     , apple_config:client_id()}
                  , {<<"client_secret">> , client_secret()}
                  , {<<"code">>          , AuthorCode}
                  , {<<"grant_type">>    , <<"authorization_code">>}
                  ],
    HttpBody    = uri_string:compose_query(DataMaps),
    Url         = apple_config:auth_token(),
    ContentType = "application/x-www-form-urlencoded",
    Request     = {Url, [], ContentType, HttpBody},
    HttpOpts    = [{timeout, 5000}],

    case httpc:request(post, Request, HttpOpts, []) of
        {ok, {_S, _H, RBody}} ->
            % ?debug("auth_token request StatusLine: ~p, HttpHeader: ~p, Body: ~p", [_S, _H, RBody]),
            RequestMap      = jsx:decode( utils:to_binary(RBody) ),
            % ?debug("auth_token RequestMap: ~p", [RequestMap]),
            RequestMap;
        Err->
            ?WARNING("generate_and_validate_tokens request err, HttpBody: ~p, Err: ~p", [HttpBody, Err]),
            throw(<<"Request auth_token error.">>)
    end.





%%------------------------------------------------------------------------------
% @doc https://developer.apple.com/documentation/sign_in_with_apple/fetch_apple_s_public_key_for_verifying_token_signature
% ps. 建议 key 放 ets 什么的
% GET https://appleid.apple.com/auth/keys
% Request 
% Content-Type: application/json
% JWKSet.Keys, id_token and identityToken
% Name            Type        PS
% alg             string      The encryption algorithm used to encrypt the token.
% e               string      The exponent value for the RSA public key.
% kid             string      A 10-character identifier key, obtained from your developer account.
% kty             string      The key type parameter setting. You must set to "RSA".
% n               string      The modulus value for the RSA public key.
% use             string      The intended use for the public key.
auth_keys(KeyId) ->
    Url      = apple_config:auth_keys(),
    Headers  = [],
    Request  = {Url, Headers},
    HttpOpts = [{timeout, 5000}],
    case httpc:request(get, Request, HttpOpts, []) of
        {ok, {_S, _H, Body}} ->
            RequestMap  = jsx:decode( utils:to_binary(Body) ),
            Keys        = maps:get(<<"keys">>, RequestMap, []),
            {_, Key}    = lists:keyfind(KeyId, 1, [jwk2key(JWK) || JWK <- Keys]),
            Key;
        Err ->
            ?debug("url: ~p, Err: ~p", [Url, Err]),
            throw(<<"Fetch Apple’s public key Err.">>)
    end.

jwk2key(JWK) ->
    Kid = maps:get(<<"kid">>,  JWK, undefined),
    N   = maps:get(<<"n">>,    JWK, undefined),
    E   = maps:get(<<"e">>,    JWK, undefined),
    Key = #'RSAPublicKey'{ modulus        = binary:decode_unsigned(base64:decode(N, #{padding => false, mode => urlsafe}))
                         , publicExponent = binary:decode_unsigned(base64:decode(E, #{padding => false, mode => urlsafe}))
                         },
    {Kid, Key}.



%%------------------------------------------------------------------------------
% @doc https://developer.apple.com/documentation/accountorganizationaldatasharing/creating-a-client-secret
client_secret() ->
    Headers = #{ alg => <<"ES256">>
               , kid => apple_config:key_id()
               },
    TS      = utils:timestamp(),
    Payload = #{ iss => apple_config:team_id()
               , iat => TS
               , exp => TS + 15552000 % 86400 * 180
               , aud => <<"https://appleid.apple.com">>
               , sub => apple_config:client_id()
               },
    H       = base64:encode( jsx:encode(Headers), #{padding => false, mode => urlsafe} ),
    P       = base64:encode( jsx:encode(Payload), #{padding => false, mode => urlsafe} ),

    Content = <<H/binary, ".", P/binary>>,
    PriKey  = apple_config:private_key(),

    [Entry] = public_key:pem_decode(PriKey),
    RSAKey  = public_key:pem_entry_decode(Entry),
    SignBin = public_key:sign(Content, sha256, RSAKey),
    SignRaw = raw(SignBin),
    SignB64 = base64:encode(SignRaw, #{padding => false, mode => urlsafe}),

    Secret  = <<Content/binary, ".", SignB64/binary>>,
    Secret.

% 参考 https://github.com/artemeff/jwt
% Transcodes the JCA ASN.1/DER-encoded signature into the concatenated R + S format
% jwt_ecdsa:signature/3
raw(Der) ->
    #'ECDSA-Sig-Value'{ r = R, s = S } = public_key:der_decode('ECDSA-Sig-Value', Der),
    RBin = int_to_bin(R),
    SBin = int_to_bin(S),
    <<RBin/binary, SBin/binary>>.

%% @private
int_to_bin(X) ->
    int_to_bin_pos(X, []).

%% @private
int_to_bin_pos(0, Ds = [_|_]) ->
    list_to_binary(Ds);
int_to_bin_pos(X, Ds) ->
    int_to_bin_pos(X bsr 8, [(X band 255)|Ds]).

