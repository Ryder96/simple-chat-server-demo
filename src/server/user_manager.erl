-module(user_manager).

-export([user_manager/2]).
-include("shared/mess_interface.hrl").

user_manager(MessageHandler, Users) ->
    receive
        {Socket, login, Name} ->
            case lists:keyfind(Name, #user_state.name, Users) of
                false ->
                    io:format("welcome ~p~n", [Name]),
                    New_User_List = [
                        #user_state{
                            name = Name,
                            socket = Socket
                        }
                        | Users
                    ],
                    MessageHandler ! {Socket, #ok{info = logged}},
                    user_manager(MessageHandler, New_User_List);
                User ->
                    io:format("~p~n", [User]),
                    io:format("someone with the ~p is already logged at ~p change name~n", [
                        User#user_state.name, User#user_state.socket
                    ]),
                    MessageHandler ! {Socket, #error{message = "username already used"}},
                    user_manager(MessageHandler, Users)
            end;
        {Socket, logout} ->
            case lists:keytake(Socket, #user_state.socket, Users) of
                false ->
                    user_manager(MessageHandler, Users);
                {_, _, New_User_List} ->
                    user_manager(MessageHandler, New_User_List)
            end
    end,
    user_manager(MessageHandler, Users).