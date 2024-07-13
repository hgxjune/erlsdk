erlsdk
=====

使用 erlang OTP 26 接入一些常用的 sdk，主要是一些请求逻辑、验签等功能。

微信 wechat 接入
-----
```erlang
% 完成以下接口，以及充值回调 callback
url_token()         -> "https://api.weixin.qq.com/cgi-bin/token".
url_code2session()  -> "https://api.weixin.qq.com/sns/jscode2session".
url_jsapi()         -> "https://api.mch.weixin.qq.com/v3/pay/transactions/jsapi".
url_close()         -> "https://api.mch.weixin.qq.com/v3/pay/transactions/out-trade-no/~s/close".
url_certificates()  -> "https://api.mch.weixin.qq.com/v3/certificates".

callback()          -> <<"https://xxxx.yyyyy.com/sdk/wechat/pay/callback">>.
```
重点：
1.  解密回调报文：erlang 对 AEAD_AES_256_GCM 的解密，需要手工设置 DecryptTag，详见`wechat_tool:aead_aes_256_gcm_decrypt/1`
2.  支付结果通知《支付API v3签名》验签：对 x509 SHA256 with RSA 格式的证书操作，需要解码。详见`wechat_tool:rsa_sha256_verify/3`。


苹果 apple 接入
-----
```erlang
url_auth_token()    -> "https://appleid.apple.com/auth/token".
url_auth_keys()     -> "https://appleid.apple.com/auth/keys".

url_test_sandbox()  -> "https://api.storekit-sandbox.itunes.apple.com/inApps/v1/notifications/test".
url_test_produce()  -> "https://api.storekit.itunes.apple.com/inApps/v1/notifications/test".
```
重点：
1.  登陆接入 Sign in with Apple REST API 的两种验证方式。
2.  支付接入 App Store Server Notifications V2，附测试方法。
3.  接入难点是 jwt 和 jws 的签名和验签，erlang 需要自行编码，请善用 jwt.io 工具。
4.  各种配置填写也是较为头痛，配置方法如下。
    1.  登陆使用 key 配置说明 [Create a private key to access a service](https://developer.apple.com/help/account/manage-keys/create-a-private-key/)
    2.  内购使用 key 配置说明 [Generate keys for in-app purchases](https://developer.apple.com/help/app-store-connect/configure-in-app-purchase-settings/generate-keys-for-in-app-purchases)
    3.  App Store Server Notifications V2 配置说明 [Enter server URLs for App Store Server Notifications](https://developer.apple.com/help/app-store-connect/configure-in-app-purchase-settings/enter-server-urls-for-app-store-server-notifications)


