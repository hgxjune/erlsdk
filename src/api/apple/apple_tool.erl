%%------------------------------------------------------------------------------
% @doc apple_tool module
% @author hgx <hgx@live.cn>
% @copyright 2024 hgx, All rights reserved.
% @since 2024-07-13 
%%------------------------------------------------------------------------------
-module(apple_tool).
-include("debug.hrl").
-include_lib("public_key/include/public_key.hrl").

-export([jwt/3]).

%% public functions
%%------------------------------------------------------------------------------
% jwt 编码，使用 RFC 7519 - https://www.rfc-editor.org/rfc/rfc7519
% @doc https://developer.apple.com/documentation/appstoreserverapi/generating_json_web_tokens_for_api_requests
jwt(Headers, Payload, PriKey) ->
    H       = base64:encode( jsx:encode(Headers), #{padding => false, mode => urlsafe} ),
    P       = base64:encode( jsx:encode(Payload), #{padding => false, mode => urlsafe} ),

    Content = <<H/binary, ".", P/binary>>,
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



%% internal functions

