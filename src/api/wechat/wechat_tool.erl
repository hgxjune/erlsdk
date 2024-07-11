%%------------------------------------------------------------------------------
% @doc wechat_tool module
% @author hgx <hgx@live.cn>
% @copyright 2024 hgx, All rights reserved.
% @since 2024-07-11 
%%------------------------------------------------------------------------------
-module(wechat_tool).
-include("debug.hrl").
-include_lib("public_key/include/public_key.hrl").

-export([authorization/5]).
-export([client_pay_sign/4]).
-export([certificates/0]).
-export([rsa_sha256_sign/2]).
-export([rsa_sha256_verify/3]).
-export([aead_aes_256_gcm_decrypt/1]).


%% public functions
%% -----------------------------------------------------------------------------
% doc https://pay.weixin.qq.com/docs/merchant/development/interface-rules/signature-generation.html
authorization(Method, Url, TS, Nonce, HttpBody) ->
    UrlMap          = uri_string:parse(Url),
    CanonicalUrl    = maps:get(path, UrlMap, ""),
    Message         = lists:concat([ utils:to_list(Method),       "\n"
                                   , utils:to_list(CanonicalUrl), "\n"
                                   , utils:to_list(TS),           "\n"
                                   , utils:to_list(Nonce),        "\n"
                                   , utils:to_list(HttpBody),     "\n"
                                   ]),
    Signature       = rsa_sha256_sign(Message, wechat_config:prikey()),
    Authorization   = io_lib:format( "WECHATPAY2-SHA256-RSA2048 mchid=~p,nonce_str=~p,timestamp=~p,serial_no=~p,signature=~p", 
                                   [ utils:to_list(wechat_config:mchid())
                                   , utils:to_list(Nonce)
                                   , utils:to_list(TS)
                                   , utils:to_list(wechat_config:serial_no())
                                   , utils:to_list(Signature)
                                   ]),
    utils:to_binary(Authorization).



%% -----------------------------------------------------------------------------
%% 客户端计算 paySign
% @doc https://pay.weixin.qq.com/wiki/doc/apiv3/apis/chapter3_5_4.shtml#menu1
% 签名串一共有四行，每一行为一个参数。行尾以\n（换行符，ASCII编码值为0x0A）结束，包括最后一行。
% 如果参数本身以\n结束，也需要附加一个\n
% 参与签名字段及格式：
% 小程序appId
% 时间戳
% 随机字符串
% 订单详情扩展字符串
% 计算签名值

% 绝大多数编程语言提供的签名函数支持对签名数据 进行签名。强烈建议商户调用该类函数，使用商户私钥对待签名串进行SHA256 with RSA签名，
% 并对签名结果进行Base64编码得到签名值
client_pay_sign(AppId, TS, Nonce, PrepayId) ->
    Package = "prepay_id=" ++ utils:to_list(PrepayId),
    Message = lists:concat([ utils:to_list(AppId),      "\n"
                           , utils:to_list(TS),         "\n"
                           , utils:to_list(Nonce),      "\n"
                           , Package,                   "\n"
                           ]),
    Sign    = rsa_sha256_sign(Message, wechat_config:prikey()),
    #{ appId        =>  utils:to_binary(AppId)
     , timeStamp    =>  TS
     , nonceStr     =>  utils:to_binary(Nonce)
     , package      =>  utils:to_binary(Package)
     , signType     =>  <<"RSA">>
     , paySign      =>  utils:to_binary(Sign)
     }.



%% -----------------------------------------------------------------------------
%% 下载平台证书，建议放 ets 
% doc https://pay.weixin.qq.com/docs/merchant/apis/platform-certificate/api-v3-get-certificates/get.html
-spec certificates() -> Certificates :: binary().
certificates() ->
    Timestamp   = utils:timestamp(),
    Url         = wechat_config:url_certificates(),
    Nonce       = utils:random_string(32),

    Authorization=authorization("GET", Url, Timestamp, Nonce, <<>>),
    HttpHeaders = [ {"Accept",          "application/json"}
                  , {"User-Agent",      "server erlang"}
                  , {"Authorization",   Authorization}
                  ],

    Request     = {Url, HttpHeaders},
    HttpOpts    = [{timeout, 5000}],
    Options     = [],

    case httpc:request(get, Request, HttpOpts, Options) of
        {ok, {_S, _H, RBody}} ->
            Map         = jsx:decode(utils:to_binary(RBody)),
            case maps:get(<<"data">>, Map, []) of
                [D1 | _] ->
                    % SN  = maps:get(<<"serial_no">>,           D1, #{}),
                    EC  = maps:get(<<"encrypt_certificate">>, D1, #{}),
                    CT  = aead_aes_256_gcm_decrypt(EC),
                    CT;
                _ ->
                    ?debug("wechat_tool certificates request map: ~p", [Map]),
                    throw("authorization request null list")
            end;
        Err ->
            ?debug("wechat_tool certificates request err: ~p, url: ~p", [Err, Url]),
            throw("authorization request error")
    end.



%% -----------------------------------------------------------------------------
%% sign 和 verify 函数对，仅针对 wechat 的证书使用
rsa_sha256_sign(Content, PriKey) ->
    [Entry]     = public_key:pem_decode(PriKey),
    RSAPriKey   = public_key:pem_entry_decode(Entry),
    SignBin     = public_key:sign(Content, sha256, RSAPriKey),
    Sign        = base64:encode(SignBin),
    Sign.

rsa_sha256_verify(Content, Base64Sign, PubKey) ->
    Sign                = base64:decode(Base64Sign),
    [{'Certificate', CertificateDer, not_encrypted}] = public_key:pem_decode(PubKey),
    Certificate         = public_key:pkix_decode_cert(CertificateDer, otp),
    TbsCertificate      = Certificate#'OTPCertificate'.tbsCertificate,
    SubjectPublicKeyInfo= TbsCertificate#'OTPTBSCertificate'.subjectPublicKeyInfo,
    PublicKey           = SubjectPublicKeyInfo#'OTPSubjectPublicKeyInfo'.subjectPublicKey, % type: rsa_public_key()
    Verify              = public_key:verify(Content, sha256, Sign, PublicKey),
    Verify.



%% -----------------------------------------------------------------------------
%% aead_aes_256_gcm 解密
%% @doc https://pay.weixin.qq.com/docs/merchant/development/interface-rules/certificate-callback-decryption.html
aead_aes_256_gcm_decrypt(EncryptCertificate) ->
    Key         = wechat_config:apiv3key(),
    AAD         = maps:get(<<"associated_data">>,   EncryptCertificate, <<"">>),
    IV          = maps:get(<<"nonce">>,             EncryptCertificate, <<"">>),
    EncryptText = maps:get(<<"ciphertext">>,        EncryptCertificate, <<"">>),

    B64         = base64:decode(EncryptText, #{padding => false}),
    B64Len      = byte_size(B64),
    Pos         = B64Len - 16,
    InText      = binary:part(B64, 0, Pos),
    DecryptTag  = binary:part(B64, Pos, 16),

    Certificate = crypto:crypto_one_time_aead(aes_256_gcm, Key, IV, InText, AAD, DecryptTag, false),
    % ?debug("DecryptResource: ~p", [Certificate]),
    Certificate.



%% internal functions

