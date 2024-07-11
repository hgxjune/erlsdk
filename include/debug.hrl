%%%-------------------------------------------------------------------
%% @doc debug.hrl
%% @end
%%%-------------------------------------------------------------------
-ifndef(DEBUG_HRL).
-define(DEBUG_HRL,true).

-include_lib("kernel/include/logger.hrl").

-define(CRITICAL(Msg),          ?LOG_CRITICAL(Msg)).
-define(CRITICAL(Msg, Args),    ?LOG_CRITICAL(Msg, Args)).
-define(ERROR(Msg),             ?LOG_ERROR(Msg)).
-define(ERROR(Msg, Args),       ?LOG_ERROR(Msg, Args)).
-define(WARNING(Msg),           ?LOG_WARNING(Msg)).
-define(WARNING(Msg, Args),     ?LOG_WARNING(Msg, Args)).
-define(NOTICE(Msg),            ?LOG_NOTICE(Msg)).
-define(NOTICE(Msg, Args),      ?LOG_NOTICE(Msg, Args)).
-define(INFO(Msg),              ?LOG_INFO(Msg)).
-define(INFO(Fmt, Args),        ?LOG_INFO(Fmt, Args)).

-define(debug(Format, Args),    ?LOG_NOTICE("~p:[~p] -- " ++ Format, [?MODULE, ?LINE | Args])).
-define(debug(Arg),             ?LOG_NOTICE("~p:[~p] -- ~p",         [?MODULE, ?LINE, Arg])).
-define(debug,                  ?LOG_NOTICE("~p:[~p] --",            [?MODULE, ?LINE])).

-endif. %% DEBUG_HRL