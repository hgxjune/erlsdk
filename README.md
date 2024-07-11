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

