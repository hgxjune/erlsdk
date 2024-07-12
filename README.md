erlsdk
=====

使用 erlang 接入一些常用的 sdk，主要是一些请求逻辑、验签等功能。

微信接入
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


apple 接入
-----
```erlang
```




