-module(user_interface).

-export([login/1, logout/0]).

-export([create_room/1, destroy_room/1, enter_room/1, exit_room/0, list_rooms/0]).

-export([send_message/1, whisper/2]).

-include("../shared/mess_interface.hrl").

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
create_room(RoomName) ->
    case check_client_is_running() of
        true -> mess_client ! {create_room, RoomName};
        false -> error("client is not running")
    end.

destroy_room(RoomName) ->
    case check_client_is_running() of
        true -> mess_client ! {destroy_room, RoomName};
        false -> error("client is not running")
    end.

enter_room(RoomName) ->
    case check_client_is_running() of
        true -> mess_client ! {enter_room, RoomName};
        false -> error("client is not running")
    end.

exit_room() ->
    case check_client_is_running() of
        true -> mess_client ! exit_room;
        false -> error("client is not running")
    end.

list_rooms() ->
    case check_client_is_running() of
        true -> mess_client ! list_rooms;
        false -> error("client is not running")
    end.

send_message(Message) ->
    case check_client_is_running() of
        true -> mess_client ! {message, Message};
        false -> error("client is not running")
    end.

whisper(Dst, Message) ->
    case check_client_is_running() of
        true -> mess_client ! {whisper, Dst, Message};
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
