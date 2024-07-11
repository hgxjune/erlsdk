%%------------------------------------------------------------------------------
% @doc wechat_user module
% @author hgx <hgx@live.cn>
% @copyright 2024 hgx, All rights reserved.
% @since 2024-07-11 
%%------------------------------------------------------------------------------
-module(wechat_user).
-include("debug.hrl").

-export([login/1]).

%% public functions
% @doc 登陆流程 https://developers.weixin.qq.com/minigame/dev/guide/open-ability/login.html
% @doc 凭证校验 https://developers.weixin.qq.com/minigame/dev/api-backend/open-api/login/auth.code2Session.html
% 
% 登录凭证校验。
% GET https://api.weixin.qq.com/sns/jscode2session?appid=APPID&secret=SECRET&js_code=JSCODE&grant_type=authorization_code

% 请求参数
% 属性          类型        默认值   必填      说明
% appid         string              是       小程序 appId
% secret        string              是       小程序 appSecret
% js_code       string              是       登录时获取的 code
% grant_type    string              是       授权类型，此处只需填写 authorization_code

% 返回值 JSON 数据包
% 属性          类型        说明
% openid        string      用户唯一标识
% session_key   string      会话密钥
% unionid       string      用户在开放平台的唯一标识符，若当前小程序已绑定到微信开放平台账号下会返回，详见 UnionID 机制说明。
% errcode       number      错误码
% errmsg        string      错误信息

% errcode 的合法值
% 值         说明  最低版本
% -1        系统繁忙，此时请开发者稍候再试 
% 0         请求成功    
% 40029     code 无效 
% 45011     频率限制，每个用户每分钟100次    
% 40226     高风险等级用户，小程序登录拦截 。风险等级详见用户安全解方案

login(Body) ->
    % ?debug("wechat login, Body: ~p", [Body]),
    Data     = jsx:decode(Body),
    JSCode   = maps:get(<<"js_code">>, Data, <<"">>),
    DataList = [ {"grant_type", "client_credential"}
               , {"appid",      wechat_config:appid()}
               , {"secret",     wechat_config:secret()}
               , {"js_code",    JSCode}
               ],

    Url      = lists:concat([wechat_config:url_code2session(), "?", uri_string:compose_query(DataList)]),
    Headers  = [],
    Request  = {Url, Headers},
    HttpOpts = [{timeout, 5000}],

    case httpc:request(get, Request, HttpOpts, []) of
        {ok, {_S, _H, RBody}} ->
            % ?debug("login httpc request url: ~p", [Url]),
            % ?debug("StatusLine: ~p, HttpHeader: ~p, RBody: ~p", [_S, _H, RBody]),
            Map       = jsx:decode(utils:to_binary(RBody)),
            OpenId    = maps:get(<<"openid">>,      Map, <<"">>),
            UnionId   = maps:get(<<"unionid">>,     Map, <<"">>),
            SessionKey= maps:get(<<"session_key">>, Map, <<"">>),
            ErrCode   = maps:get(<<"errcode">>,     Map, 0),
            ErrMsg    = maps:get(<<"errmsg">>,      Map, <<"SUCCESS">>),

            ?debug("wechat login request openid     : ~p", [OpenId]),
            ?debug("wechat login request unionid    : ~p", [UnionId]),
            ?debug("wechat login request session_key: ~p", [SessionKey]),
            ?debug("wechat login request errcode    : ~p", [ErrCode]),
            ?debug("wechat login request errmsg     : ~p", [ErrMsg]),

            %%
            %% 登陆逻辑
            %%

            jsx:encode( #{ errcode  => ErrCode
                         , errmsg   => ErrMsg
                         , openid   => OpenId
                         } );
        Err ->
            ?debug("login httpc request err: ~p, url: ~p", [Err, Url]),
            jsx:encode( #{ errcode  => 999999
                         , errmsg   => <<"unkonw err.">>
                         , openid   => <<"">>
                         } )
    end.


%% internal functions

