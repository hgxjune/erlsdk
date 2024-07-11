%%------------------------------------------------------------------------------
% @doc erlsdk_socket module
% @author hgx <hgx@live.cn>
% @copyright 2024 hgx, All rights reserved.
% @since 2024-07-11 
%%------------------------------------------------------------------------------
-module(erlsdk_socket).

-export([start/0, stop/0]).
-export([reload_dispatch/0]).

%% public functions
start() ->
    Config = application:get_all_env(erlsdk),
    TransOpts = get_trans_opts(Config),
    ProtoOpts = get_proto_opts(Config),

    {ok, _} = cowboy:start_clear(?MODULE, TransOpts, ProtoOpts),
    ok.

stop() ->
    cowboy:stop_listener(http),
    ok.

reload_dispatch() ->
    Dispatch = init_dispatch(),
    cowboy:set_env(?MODULE, dispatch, Dispatch),
    ok.



%% internal functions
get_trans_opts(Config) ->
    #{ max_connections  => proplists:get_value(max_connections, Config)
     , num_acceptors    => proplists:get_value(num_acceptors,   Config)
     , num_conns_sups   => proplists:get_value(num_conns_sups,  Config)
     , socket_opts      => [{port, proplists:get_value(port,    Config)}]
     }.

get_proto_opts(_Config) ->
    Dispatch = init_dispatch(),
    %% 大量路由可以使用优化
    %% persistent_term:put ( my_app_dispatch , Dispatch ),
    #{ env => #{ dispatch => Dispatch }
     }.

%% 
init_dispatch() ->
    Host1 = {'_', init_path()},
    Router = [ Host1 ],
    cowboy_router:compile(Router).

init_path() ->
    [ {"/sdk/apple/[...]",      route_apple,        []}
    , {"/sdk/wechat/[...]",     route_wechat,       []}
    , {"/[...]",                route_root,         []}
    ].

