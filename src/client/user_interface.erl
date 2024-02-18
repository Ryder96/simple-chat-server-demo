-module(user_interface).

-export([login/1, logout/0]).

-include("shared/mess_interface.hrl").

login(Name) ->
    case check_client_is_running() of
        true -> mess_client ! #login{username = Name};
        false -> error("client is not running")
    end.

logout() ->
    case check_client_is_running() of
        true -> mess_client ! logout;
        false -> error("client is not running")
    end.

check_client_is_running() ->
    case whereis(mess_client) of
        undefined ->
            io:format("start chat client with: chat_client:start() before~n", []),
            false;
        _ ->
            true
    end.
