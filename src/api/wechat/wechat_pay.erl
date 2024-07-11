%%------------------------------------------------------------------------------
% @doc wechat_pay module
% @author hgx <hgx@live.cn>
% @copyright 2024 hgx, All rights reserved.
% @since 2024-07-11 
%%------------------------------------------------------------------------------
-module(wechat_pay).
-include("debug.hrl").

-export([create/1]).
-export([close/1]).
-export([callback/2]).

%% public functions
%%------------------------------------------------------------------------------
% 创建订单
% @doc 统一流程 https://developers.weixin.qq.com/miniprogram/dev/api/payment/wx.requestPayment.html
% @doc 流程图   https://pay.weixin.qq.com/wiki/doc/apiv3/open/pay/chapter2_3.shtml 

%% public functions
% @doc JSAPI下单 https://pay.weixin.qq.com/wiki/doc/apiv3/apis/chapter3_1_1.shtml
% POST https://api.mch.weixin.qq.com/v3/pay/transactions/jsapi

% 请求参数
% 参数名               变量          类型[长度限制]    必填  描述
% 应用ID               appid         string[1,32]      是    body 由微信生成的应用ID，全局唯一。请求基础下单接口时请注意APPID的应用属性，例如公众号场景下，需使用应用属性为公众号的服务号APPID
% 直连商户号           mchid         string[1,32]      是    body 直连商户的商户号，由微信支付生成并下发。
% 商品描述             description   string[1,127]     是    body 商品描述
% 商户订单号           out_trade_no  string[6,32]      是    body 商户系统内部订单号，只能是数字、大小写字母_-*且在同一个商户号下唯一
% 通知地址             notify_url    string[1,256]     是    body异步接收微信支付结果通知的回调地址，通知url必须为外网可访问的url，不能携带参数。 公网域名必须为https，如果是走专线接入，使用专线NAT IP或者私有回调域名可使用http
% 订单金额             amount        object            是    body 订单金额信息
%     参数名           变量          类型[长度限制]    必填  描述
%     总金额           total         int               是    订单总金额，单位为分。
%     货币类型         currency      string[1,16]      否    CNY：人民币，境内商户号仅支持人民币。
% 支付者               payer         object            是    body 支付者信息
%     参数名           变量          类型[长度限制]    必填  描述
%     用户标识         openid        string[1,128]     是    用户在直连商户appid下的唯一标识。 下单前需获取到用户的Openid，Openid获取详见

% 返回参数
% 参数名               变量          类型[长度限制]    必填  描述
% 预支付交易会话标识   prepay_id     string[1,64]      是    预支付交易会话标识。用于后续接口调用中使用，该值有效期为2小时
create(BodyIn) ->
    DataIn      = jsx:decode(BodyIn),
    OpenId      = maps:get(<<"openid">>,    DataIn, undefined),
    ItemId      = maps:get(<<"itemid">>,    DataIn, 0),
    if
        OpenId == undefined -> throw(<<"No openid found.">>);
        ItemId == 0         -> throw(<<"No itemid found.">>);
        true -> ok
    end,
    Desc        = <<"100块都不给我">>,
    Rmb         = 100, % 
    AppId       = wechat_config:appid(),
    MchId       = wechat_config:mchid(),
    TradeNo     = utils:random_string(32), % 建议用 uuid 之类的保证不重复
    TS          = utils:timestamp(),
    Nonce       = utils:random_string(32),

    DataMaps    = #{ <<"appid">>            => AppId
                   , <<"mchid">>            => MchId
                   , <<"description">>      => Desc
                   , <<"out_trade_no">>     => TradeNo
                   , <<"notify_url">>       => wechat_config:callback()
                   , <<"amount">>           => #{ <<"total">>       => Rmb
                                                , <<"currency">>    => <<"CNY">>
                                                }
                   , <<"payer">>            => #{ <<"openid">>      => OpenId }
                   },

    Url         = wechat_config:url_jsapi(),
    HttpBody    = jsx:encode(DataMaps),
    ContentType = "application/json",
    Authorization=wechat_tool:authorization("POST", Url, TS, Nonce, HttpBody),
    HttpHeaders = [ {"Accept",          "application/json"}
                  , {"User-Agent",      "server erlang"}
                  , {"Authorization",   Authorization}
                  ],
    Request     = {Url, HttpHeaders, ContentType, HttpBody},
    HttpOpts    = [{timeout, 5000}],
    Options     = [],

    case httpc:request(post, Request, HttpOpts, Options) of
        {ok, {_S, _H, RBody}} ->
            RMap        = jsx:decode(utils:to_binary(RBody)),
            PrepayId    = maps:get(<<"prepay_id">>, RMap, undefined),
            if
                undefined == PrepayId ->
                    Code        = maps:get(<<"code">>,      RMap, undefined),
                    Message     = maps:get(<<"message">>,   RMap, undefined),
                    jsx:encode( #{ code     => Code
                                 , message  => Message
                                 } );
                true ->
                    %%
                    %% 业务逻辑
                    %%
                    jsx:encode( #{ code => 0
                                 , data => #{ out_trade_no  => TradeNo
                                            , prepay_id     => PrepayId
                                            }
                                 , sign => wechat_tool:client_pay_sign(AppId, TS, Nonce, PrepayId)
                                 } )
            end;
        Err->
            ?WARNING("wechat_pay jsapi request err, HttpBody: ~p, Err: ~p", [HttpBody, Err]),
            throw(<<"Request jsapi error.">>)
    end.


%%------------------------------------------------------------------------------
% 关闭订单
% doc https://pay.weixin.qq.com/wiki/doc/apiv3/apis/chapter3_1_3.shtml
% 请求URL： https://api.mch.weixin.qq.com/v3/pay/transactions/out-trade-no/{out_trade_no}/close
% 请求方式： POST
% 参数名       变量              类型[长度限制]      必填     描述
% 直连商户号   mchid             string[1,32]        是       body 直连商户的商户号，由微信支付生成并下发。
% 商户订单号   out_trade_no      string[6,32]        是       path 商户系统内部订单号，只能是数字、大小写字母_-*且在同一个商户号下唯一

close(BodyIn) ->
    DataIn      = jsx:decode(BodyIn),
    TradeNo     = maps:get(<<"out_trade_no">>, DataIn, undefined),
    if
        TradeNo == undefined        -> throw(<<"out_trade_no not found.">>);
        true -> ok
    end,

    TS          = utils:timestamp(),
    Nonce       = utils:random_string(32),
    Url         = io_lib:format( wechat_config:url_close(), [TradeNo] ),
    HttpBody    = jsx:encode( #{ <<"mchid">>    => wechat_config:mchid() } ),
    Authorization=wechat_tool:authorization("POST", Url, TS, Nonce, HttpBody),
    HttpHeaders = [ {"Accept",          "application/json"}
                  , {"User-Agent",      "server erlang"}
                  , {"Authorization",   Authorization}
                  ],
    ContentType = "application/json",

    Request     = {Url, HttpHeaders, ContentType, HttpBody},
    HttpOpts    = [{timeout, 5000}],
    Options     = [],

    case httpc:request(post, Request, HttpOpts, Options) of
        {ok, {_S, _H, RBody}} ->
            ?debug("Body: ~p", [RBody]),
            %%
            %% 业务逻辑
            %%
            jsx:encode( #{ code => 0
                         , data => #{}
                         } );
        Err->
            ?WARNING("wechat_pay jsapi request err, HttpBody: ~p, Err: ~p", [HttpBody, Err]),
            throw(<<"Request jsapi error.">>)
    end.





%% -----------------------------------------------------------------------------
% 支付通知API
% 请求方式：POST
% 参数名              变量              类型[长度限制]     必填  描述
% 通知ID              id                string[1,36]       是    通知的唯一ID
% 通知创建时间        create_time       string[1,32]       是    通知创建的时间，遵循rfc3339标准格式，格式为yyyy-MM-DDTHH:mm:ss+TIMEZONE，yyyy-MM-DD表示年月日，T出现在字符串中，表示time元素的开头，HH:mm:ss.表示时分秒，TIMEZONE表示时区（+08:00表示东八区时间，领先UTC 8小时，即北京时间）。例如：2015-05-20T13:29:35+08:00表示北京时间2015年05月20日13点29分35秒。
% 通知类型            event_type        string[1,32]       是    通知的类型，支付成功通知的类型为TRANSACTION.SUCCESS
% 通知数据类型        resource_type     string[1,32]       是    通知的资源数据类型，支付成功通知为encrypt-resource
% 通知数据            resource          object             是    通知资源数据
%     参数名          变量              类型[长度限制]     必填  描述
%     加密算法类型    algorithm         string[1,32]       是    对开启结果数据进行加密的加密算法，目前只支持AEAD_AES_256_GCM
%     数据密文        ciphertext        string[1,1048576]  是    Base64编码后的开启/停用结果数据密文
%     附加数据        associated_data   string[1,16]       否    附加数据
%     原始类型        original_type     string[1,16]       是    原始回调类型，为transaction
%     随机串          nonce             string[1,16]       是    加密使用的随机串
% 回调摘要            summary           string[1,64]       是    回调摘要

% 商户对resource对象进行解密后，得到的资源对象示例
% 参数名              变量              类型[长度限制]     必填  描述
% 应用ID              appid             string[1,32]       是    直连商户申请的公众号或移动应用appid。
% 商户号              mchid             string[1,32]       是    商户的商户号，由微信支付生成并下发。
% 商户订单号          out_trade_no      string[6,32]       是    商户系统内部订单号，只能是数字、大小写字母_-*且在同一个商户号下唯一。
% 微信支付订单号      transaction_id    string[1,32]       是    微信支付系统生成的订单号。
% 交易类型            trade_type        string[1,16]       是    交易类型，枚举值： JSAPI APP MWEB
% 交易状态            trade_state       string[1,32]       是    交易状态，枚举值： SUCCESS
% 交易状态描述        trade_state_desc  string[1,256]      是    交易状态描述
% 付款银行            bank_type         string[1,32]       是    银行类型，采用字符串类型的银行标识。银行标识请参考《银行类型对照表》
% 附加数据            attach            string[1,128]      否    附加数据，在查询API和支付通知中原样返回，可作为自定义参数使用，实际情况下只有支付完成状态才会返回该字段。
% 支付完成时间        success_time      string[1,64]       是    支付完成时间，遵循rfc3339标准格式，
% 支付者              payer             object             是    支付者信息
%     参数名          变量              类型[长度限制]     必填  描述
%     用户标识        openid            string[1,128]      是    用户在直连商户appid下的唯一标识。
% 订单金额            amount            object             是    订单金额信息
%     参数名          变量              类型[长度限制]     必填  描述
%     总金额          total             int                是    订单总金额，单位为分。
%     用户支付金额    payer_total       int                是    用户支付金额，单位为分。
%     货币类型        currency          string[1,16]       是    CNY：人民币，境内商户号仅支持人民币。
%     用户支付币种    payer_currency    string[1,16]       是    用户支付币种

% 通知应答
% 接收成功：HTTP应答状态码需返回200或204，无需返回应答报文。
% 接收失败：HTTP应答状态码需返回5XX或4XX，同时需返回应答报文，格式如下：
% 参数名              变量              类型[长度限制]     必填  描述
% 返回状态码          code              string[1,32]       是    错误码，SUCCESS为清算机构接收成功，其他错误码为失败。
% 返回信息            message           string[1,64]       是    返回信息，如非空，为错误原因。
callback(Req0, Body) ->
    try
        true = verify_signature(Req0, Body),
        ok   = pay_item(Body),
        diviner_web:reply_200(Req0, jsx:encode(#{ code => <<"SUCCESS">>, message => <<"SUCCESS">>}))
    catch
        throw:Reason:_Stack ->
            ?debug("wechat_pay callback throw: ~p", [Reason]),
            diviner_web:reply_200(Req0, jsx:encode(#{ code => <<"SUCCESS">>, message => <<"SUCCESS">>}));
        Class:Reason:Stack ->
            ?debug("wechat_pay callback error, Class: ~p, Reason: ~p, Stack: ~p", [Class, Reason, Stack]),
            diviner_web:reply_200(Req0, jsx:encode(#{ code => <<"SUCCESS">>, message => <<"SUCCESS">>}))
    end.


%% internal functions
%% -----------------------------------------------------------------------------
verify_signature(Req0, Body) ->
    PubKey      = wechat_tool:certificates(),
    Nonce       = cowboy_req:header(<<"wechatpay-nonce">>,          Req0, undefined),
    Serial      = cowboy_req:header(<<"wechatpay-serial">>,         Req0, undefined),
    Signature   = cowboy_req:header(<<"wechatpay-signature">>,      Req0, undefined),
    SignType    = cowboy_req:header(<<"wechatpay-signature-type">>, Req0, undefined),
    Timestamp   = cowboy_req:header(<<"wechatpay-timestamp">>,      Req0, undefined),
    ?debug("wechatpay-nonce             : ~p", [Nonce]),
    ?debug("wechatpay-serial            : ~p", [Serial]),
    ?debug("wechatpay-Signature         : ~p", [Signature]),
    ?debug("wechatpay-Signature-type    : ~p", [SignType]),
    ?debug("wechatpay-timestamp         : ~p", [Timestamp]),

    LineBreaks  = <<"\n">>,
    Message     = << Timestamp/binary,  LineBreaks/binary
                   , Nonce/binary,      LineBreaks/binary
                   , Body/binary,       LineBreaks/binary
                  >>,
    % ?debug("Message                     : ~n~ts", [Message]),
    Verify      = wechat_tool:rsa_sha256_verify(Message, Signature, PubKey),
    Verify.


%% -----------------------------------------------------------------------------
pay_item(BodyIn) ->
    ?debug("callback begin"),
    DataIn          = jsx:decode(BodyIn),
    
    Id              = maps:get(<<"id">>,                DataIn,     <<"">>),            % 通知ID
    CreateTime      = maps:get(<<"create_time">>,       DataIn,     <<"">>),            % 通知创建时间
    EventType       = maps:get(<<"event_type">>,        DataIn,     <<"">>),            % 通知类型
    ResourceType    = maps:get(<<"resource_type">>,     DataIn,     <<"">>),            % 通知数据类型
    Summary         = maps:get(<<"summary">>,           DataIn,     <<"">>),            % 回调摘要
    Resource        = maps:get(<<"resource">>,          DataIn,     #{}),               % 通知数据
    Algorithm       = maps:get(<<"algorithm">>,         Resource,   <<"">>),            % 加密算法类型
    Ciphertext      = maps:get(<<"ciphertext">>,        Resource,   <<"">>),            % 数据密文
    AssociatedData  = maps:get(<<"associated_data">>,   Resource,   <<"">>),            % 附加数据
    OriginalType    = maps:get(<<"original_type">>,     Resource,   <<"">>),            % 原始类型
    Nonce           = maps:get(<<"nonce">>,             Resource,   <<"">>),            % 随机串

    ?debug("DataIn        : ~p", [DataIn        ]),
    ?debug("Id            : ~p", [Id            ]),
    ?debug("CreateTime    : ~p", [CreateTime    ]),
    ?debug("EventType     : ~p", [EventType     ]),
    ?debug("ResourceType  : ~p", [ResourceType  ]),
    ?debug("Summary       : ~ts",[Summary       ]),
    ?debug("Algorithm     : ~p", [Algorithm     ]),
    ?debug("Ciphertext    : ~p", [Ciphertext    ]),
    ?debug("AssociatedData: ~p", [AssociatedData]),
    ?debug("OriginalType  : ~p", [OriginalType  ]),
    ?debug("Nonce         : ~p", [Nonce         ]),

    DecryptResource = wechat_tool:aead_aes_256_gcm_decrypt(Resource),
    Json            = jsx:decode(DecryptResource),

    Appid           = maps:get(<<"appid">>,             Json,       <<"">>),
    Mchid           = maps:get(<<"mchid">>,             Json,       <<"">>),
    OutTradeNo      = maps:get(<<"out_trade_no">>,      Json,       <<"">>),
    TransactionId   = maps:get(<<"transaction_id">>,    Json,       <<"">>),
    TradeType       = maps:get(<<"trade_type">>,        Json,       <<"">>),
    TradeState      = maps:get(<<"trade_state">>,       Json,       <<"">>),
    TradeStateDesc  = maps:get(<<"trade_state_desc">>,  Json,       <<"">>),
    BankType        = maps:get(<<"bank_type">>,         Json,       <<"">>),
    Attach          = maps:get(<<"attach">>,            Json,       <<"">>),
    SuccessTime     = maps:get(<<"success_time">>,      Json,       <<"">>),
    Payer           = maps:get(<<"payer">>,             Json,       #{}),
    Amount          = maps:get(<<"amount">>,            Json,       #{}),
    OpenId          = maps:get(<<"openid">>,            Payer,      <<"">>),
    Total           = maps:get(<<"total">>,             Amount,     0),
    PayerTotal      = maps:get(<<"payer_total">>,       Amount,     0),
    Currency        = maps:get(<<"currency">>,          Amount,     <<"">>),
    PayerCurrency   = maps:get(<<"payer_currency">>,    Amount,     <<"">>),
    ?debug("--"),
    ?debug("Appid         : ~p", [Appid         ]),
    ?debug("Mchid         : ~p", [Mchid         ]),
    ?debug("OutTradeNo    : ~p", [OutTradeNo    ]),
    ?debug("TransactionId : ~p", [TransactionId ]),
    ?debug("TradeType     : ~p", [TradeType     ]),
    ?debug("TradeState    : ~p", [TradeState    ]),
    ?debug("TradeStateDesc: ~ts",[TradeStateDesc]),
    ?debug("BankType      : ~p", [BankType      ]),
    ?debug("Attach        : ~p", [Attach        ]),
    ?debug("SuccessTime   : ~p", [SuccessTime   ]),
    ?debug("OpenId        : ~p", [OpenId        ]),
    ?debug("Total         : ~p", [Total         ]),
    ?debug("PayerTotal    : ~p", [PayerTotal    ]),
    ?debug("Currency      : ~p", [Currency      ]),
    ?debug("PayerCurrency : ~p", [PayerCurrency ]),

    %%
    %% 业务逻辑
    %%

    ok.



