%%------------------------------------------------------------------------------
% @doc utils module
% @author hgx <hgx@live.cn>
% @copyright 2024 hgx, All rights reserved.
% @since 2024-07-11 
%%------------------------------------------------------------------------------
-module(utils).

-export([ timestamp/0
        , today/0
        ]).

-export([ to_atom/1
        , to_list/1
        , to_binary/1
        , to_int/1
        , to_bool/1
        , to_tuple/1
        ]).

-export([ random_string/1
        ]).


%%------------------------------------------------------------------------------
%% 时间戳
timestamp() ->
    erlang:system_time(second).

%% 今天起始时间戳
today() ->
    Now      = erlang:system_time(second),
    RFC3339  = calendar:system_time_to_rfc3339(Now),
    [Day, _] = string:split(RFC3339, "T"),
    calendar:rfc3339_to_system_time(Day ++ "T00:00:00+08:00").


%%------------------------------------------------------------------------------
%% @doc convert other type to atom
to_atom(Msg) when is_atom(Msg)      -> Msg;
to_atom(Msg) when is_binary(Msg)    -> list_to_atom( binary_to_list(Msg) );
to_atom(Msg) when is_list(Msg)      -> list_to_atom(Msg);
to_atom(_) -> throw(other_value).

%% @doc convert other type to list
to_list(Msg) when is_list(Msg)      -> Msg;
to_list(Msg) when is_binary(Msg)    -> binary_to_list(Msg);
to_list(Msg) when is_atom(Msg)      -> atom_to_list(Msg);
to_list(Msg) when is_integer(Msg)   -> integer_to_list(Msg);
to_list(Msg) when is_tuple(Msg)     -> tuple_to_list(Msg);
to_list(_) -> throw(other_value).

%% @doc convert other type to binary
to_binary(Msg) when is_binary(Msg)  -> Msg;
to_binary(Msg) when is_atom(Msg)    -> list_to_binary( atom_to_list(Msg) );
to_binary(Msg) when is_list(Msg)    -> list_to_binary(Msg);
to_binary(Msg) when is_integer(Msg) -> list_to_binary( integer_to_list(Msg) );
to_binary(_) -> throw(other_value).

%% @doc convert other type to integer
to_int(Msg) when is_integer(Msg)    -> Msg;
to_int(Msg) when is_binary(Msg)     -> list_to_integer( binary_to_list(Msg) );
to_int(Msg) when is_list(Msg)       -> list_to_integer(Msg);
to_int(Msg) when is_float(Msg)      -> round(Msg);
to_int(_) -> throw(other_value).

%% @doc convert other type to bool
to_bool(D) when is_boolean(D)       -> D;
to_bool(D) when is_integer(D)       -> D =/= 0;
to_bool(D) when is_list(D)          -> length(D) =/= 0;
to_bool(D) when is_binary(D)        -> to_bool( binary_to_list(D) );
to_bool(_) -> throw(other_value).

%% @doc convert other type to tuple
to_tuple(T) when is_tuple(T)        -> T;
to_tuple(T) when is_list(T)         -> list_to_tuple(T);
to_tuple(T) -> {T}.


%%%-------------------------------------------------------------------
%% 生成一个随机字符串，给数字加权
random_string(Len) ->
    Chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyz0123456789",
    [lists:nth(rand:uniform(72), Chars) || _ <- lists:seq(1, Len)].


    
%% internal functions

