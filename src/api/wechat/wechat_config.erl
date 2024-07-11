%%------------------------------------------------------------------------------
% @doc wechat_config module
% @author hgx <hgx@live.cn>
% @copyright 2024 hgx, All rights reserved.
% @since 2024-07-11 
%%------------------------------------------------------------------------------
-module(wechat_config).

-export([appid/0]).             %% AppID(小程序ID)
-export([secret/0]).            %% AppSecret(小程序密钥)
-export([mchid/0]).             %% 微信支付商户号
-export([callback/0]).          %% 充值回调 url
-export([apiv3key/0]).          %% APIv3密钥
-export([serial_no/0]).         %% 证书编号
-export([pubkey/0]).            %% 公钥
-export([prikey/0]).            %% 私钥

-export([url_token/0]).         %% url 获取接口调用凭据 
-export([url_code2session/0]).  %% url 登录凭证校验 
-export([url_jsapi/0]).         %% url 预支付 
-export([url_close/0]).         %% url 关闭订单 
-export([url_certificates/0]).  %% url 下载平台证书 

%% public functions
appid()             -> <<"wx6666666666666666">>.
secret()            -> <<"2233aaaaaaaaaaaaaaaaaaaaaaaaaaaa">>.
mchid()             -> <<"1688888888">>.
callback()          -> <<"https://xxxx.yyyyy.com/sdk/wechat/pay/callback">>.
apiv3key()          -> <<"6699bbbbbbbbbbbbbbbbbbbbbbbbbbbb">>.
serial_no()         -> <<"7788PPMMPPMMPPMMPPMMPPMMPPMMPPMMPPMMPPMM">>.
pubkey()            -> <<"-----BEGIN CERTIFICATE-----\nMII.........nn\n-----END CERTIFICATE-----">>.
prikey()            -> <<"-----BEGIN PRIVATE KEY-----\nMII.........nn\n-----END PRIVATE KEY-----">>.


url_token()         -> "https://api.weixin.qq.com/cgi-bin/token".
url_code2session()  -> "https://api.weixin.qq.com/sns/jscode2session".
url_jsapi()         -> "https://api.mch.weixin.qq.com/v3/pay/transactions/jsapi".
url_close()         -> "https://api.mch.weixin.qq.com/v3/pay/transactions/out-trade-no/~s/close".
url_certificates()  -> "https://api.mch.weixin.qq.com/v3/certificates".

%% internal functions

