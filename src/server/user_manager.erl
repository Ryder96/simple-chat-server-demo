-module(user_manager).

-export([user_manager/2]).
-include("../shared/mess_interface.hrl").

user_manager(MessageHandler, Users) ->
    receive
        {Socket, login, Name} ->
            case lists:keyfind(Name, #user_state.name, Users) of
                false ->
                    io:format("welcome ~s~n", [Name]),
                    New_User_List = [
                        #user_state{
                            name = Name,
                            socket = Socket,
                            room = lobby,
                            room_type = lobby,
                            pvt_rooms = []
                        }
                        | Users
                    ],
                    MessageHandler ! #direct_message{socket = Socket, message = #ok{info = logged}},
                    user_manager(MessageHandler, New_User_List);
                User ->
                    io:format("someone with the ~p is already logged at ~p change name~n", [
                        User#user_state.name, User#user_state.socket
                    ]),
                    MessageHandler !
                        #direct_message{
                            socket = Socket, message = #error{message = "username already used"}
                        },
                    user_manager(MessageHandler, Users)
            end;
        {Socket, logout} ->
            case lists:keytake(Socket, #user_state.socket, Users) of
                false ->
                    user_manager(MessageHandler, Users);
                {_, State, New_User_List} ->
                    rooms_manager ! {exit, State#user_state.room},
                    user_manager(MessageHandler, New_User_List)
            end;
        {Socket, exit_room, User} ->
            UserState = lists:keyfind(User, #user_state.name, Users),
            case UserState#user_state.room of
                lobby ->
                    MessageHandler !
                        #direct_message{
                            socket = Socket, message = #system{message = "you are not in a room"}
                        };
                RoomName ->
                    MessageHandler !
                        #direct_message{
                            socket = Socket,
                            message = #system{message = "you are back to the lobby!"}
                        },
                    rooms_manager ! {RoomName, exit, User},
                    Users2 = lists:map(
                        fun
                            (State) when State#user_state.name == User ->
                                NewUserState = State#user_state{room = lobby},
                                NewUserState;
                            (State) ->
                                State
                        end,
                        Users
                    ),
                    user_manager(MessageHandler, Users2)
            end;
        {Socket, invite_to_room, {Name, {Guest, RoomName}}} ->
            UserState = lists:keyfind(Guest, #user_state.name, Users),
            case UserState of
                false ->
                    MessageHandler !
                        #direct_message{
                            socket = Socket,
                            message = #system{
                                message = io_lib:format("~s is not logged in", [Guest])
                            }
                        },
                    MessageHandler !
                        #direct_message{
                            socket = Socket,
                            message = #system{
                                message = io_lib:format("~s can enter room when is online", [Guest])
                            }
                        };
                State ->
                    GuestSocket = State#user_state.socket,
                    rooms_manager ! {Socket, invite, GuestSocket, Name, Guest, RoomName}
            end;
        {authorize, Guest, RoomName} ->
            Users2 = lists:map(
                fun
                    (UserState) when UserState#user_state.name == Guest ->
                        NewUserState = UserState#user_state{
                            pvt_rooms = [RoomName | UserState#user_state.pvt_rooms]
                        },
                        NewUserState;
                    (UserState) ->
                        UserState
                end,
                Users
            ),
            user_manager(MessageHandler, Users2);
        %%% message handler
        {Socket, message, {Sender, Message}} ->
            UserState = lists:keyfind(Sender, #user_state.name, Users),
            case UserState#user_state.room of
                lobby ->
                    MessageHandler !
                        #direct_message{
                            socket = Socket, message = #error{message = "you are not in a room"}
                        };
                RoomName ->
                    rooms_manager !
                        {
                            #room{name = RoomName, type = UserState#user_state.room_type},
                            message,
                            {Sender, Message}
                        }
            end;
        {Socket, whisper, {User, Dst, Message}} ->
            case lists:keyfind(Dst, #user_state.name, Users) of
                false ->
                    MessageHandler !
                        #direct_message{
                            socket = Socket,
                            message = #system{message = io_lib:format("~s is not logged", [Dst])}
                        };
                DstState ->
                    MessageHandler !
                        #direct_message{
                            socket = DstState#user_state.socket,
                            message = #ok{
                                message = io_lib:format("[whisper] ~s: ~s", [User, Message])
                            }
                        }
            end,
            user_manager(MessageHandler, Users);
        {Socket, list_rooms, User} ->
            UserState = lists:keyfind(User, #user_state.name, Users),
            case length(UserState#user_state.pvt_rooms) of
                0 ->
                    MessageHandler !
                        #direct_message{
                            socket = Socket, message = #ok{message = "no pvt rooms available"}
                        };
                N ->
                    Names = lists:map(
                        fun(Room) ->
                            io_lib:format("~w", [Room])
                        end,
                        UserState#user_state.pvt_rooms
                    ),

                    Buffer = [io_lib:format("Private Rooms availables ~p:", [N]) | Names],
                    MessageHandler !
                        #direct_message{
                            socket = Socket, message = #ok{message = Buffer, info = print_list}
                        }
            end;
        #update_user_room{user = User, new_room = Room} ->
            Users2 = lists:map(
                fun
                    (UserState) when UserState#user_state.name == User ->
                        NewUserState = UserState#user_state{
                            room = Room#room.name, room_type = Room#room.type
                        },
                        NewUserState;
                    (UserState) ->
                        UserState
                end,
                Users
            ),
            user_manager(MessageHandler, Users2)
    end,
    user_manager(MessageHandler, Users).
