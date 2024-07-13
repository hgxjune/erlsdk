%%------------------------------------------------------------------------------
% @doc apple_config module
% @author hgx <hgx@live.cn>
% @copyright 2024 hgx, All rights reserved.
% @since 2024-07-11 
%%------------------------------------------------------------------------------
-module(apple_config).

-export([team_id/0]).
-export([client_id/0]).
-export([bundle_id/0]).
-export([issuer_id/0]).

-export([auth_key_id/0]).
-export([auth_key/0]).
-export([url_auth_token/0]).
-export([url_auth_keys/0]).

-export([purchase_key_id/0]).
-export([purchase_key/0]).
-export([url_test_sandbox/0]).
-export([url_test_produce/0]).

%% public functions
% app store connect 用户详细资料
team_id()           -> <<"BWBWBWV587">>.
issuer_id()         -> <<"57246542-96fe-1a63-e053-0824d011072a">>.
% app store connect App 信息
client_id()         -> <<"com.xxx.bilibili">>.
bundle_id()         -> client_id().

% 证书按照此页申请，证书只能下载一次，要保存好，证书例： AuthKey_63P563P588.p8
% @doc https://developer.apple.com/help/account/manage-keys/create-a-private-key/
auth_key_id()       -> <<"63P563P588">>.
auth_key()          -> <<"-----BEGIN PRIVATE KEY-----\nMIGT........P588\n-----END PRIVATE KEY-----">>.

url_auth_token()    -> "https://appleid.apple.com/auth/token".
url_auth_keys()     -> "https://appleid.apple.com/auth/keys".


% 证书按照此页申请，证书例： SubscriptionKey_2X9R4HXF34.p8
% @doc https://developer.apple.com/help/app-store-connect/configure-in-app-purchase-settings/generate-keys-for-in-app-purchases
purchase_key_id()   -> <<"2X9R4HXF34">>.
purchase_key()      -> <<"-----BEGIN PRIVATE KEY-----\nMIGT........XF34\n-----END PRIVATE KEY-----">>.

url_test_sandbox()  -> "https://api.storekit-sandbox.itunes.apple.com/inApps/v1/notifications/test".
url_test_produce()  -> "https://api.storekit.itunes.apple.com/inApps/v1/notifications/test".
%% internal functions

