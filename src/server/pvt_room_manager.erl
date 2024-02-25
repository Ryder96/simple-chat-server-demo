-module(pvt_room_manager).

-export([room_manager/5]).

-include("../shared/mess_interface.hrl").

room_manager(MessageHandler, Owner, RoomName, Authorized, Users) ->
    receive
        {enter, {Socket, User}} ->
            IsAuthorized = lists:member(User, Authorized),
            case IsAuthorized of
                true ->
                    case lists:keyfind(User, 1, Users) of
                        false ->
                            Users2 = [{User, Socket} | Users],
                            user_manager !
                                #update_user_room{
                                    user = User, new_room = #room{name = RoomName, type = private}
                                },
                            MessageHandler !
                                #broadcast_message{
                                    clients = Users,
                                    message = #ok{
                                        message = io_lib:format("~s enter the room", [User])
                                    }
                                },
                            OldMessages = chat_server_dynamodb:fetch_messages(RoomName, Owner),
                            Messages = lists:map(
                                fun(Message) ->
                                    Message#message_ddb.sender ++ ": " ++ Message#message_ddb.body
                                end,
                                OldMessages
                            ),

                            MessageHandler !
                                #direct_message{
                                    socket = Socket, message = #ok{message = "you entered the room"}
                                },

                            MessageHandler !
                                #direct_message{
                                    socket = Socket,
                                    message = #ok{message = Messages, info = print_list}
                                },
                            room_manager(MessageHandler, Owner, RoomName, Authorized, Users2);
                        _ ->
                            MessageHandler !
                                #direct_message{
                                    socket = Socket, message = #system{message = "already in room"}
                                }
                    end;
                false ->
                    MessageHandler !
                        #direct_message{
                            socket = Socket, message = #system{message = "you are not authorized"}
                        }
            end;
        {exit, User} ->
            Users2 = lists:keydelete(User, 1, Users),
            MessageHandler !
                #broadcast_message{
                    clients = Users,
                    message =
                        #system{message = io_lib:format("~s exit the room", [User])}
                },
            room_manager(MessageHandler, Owner, RoomName, Authorized, Users2);
        {authorize, HostSocket, Guest} ->
            case lists:keyfind(Guest, 1, Authorized) of
                false ->
                    user_manager ! {authorize, Guest, RoomName},
                    AuthorizedUpdated = [Guest | Authorized],
                    chat_server_dynamodb:authorize(RoomName, Owner, Guest),
                    room_manager(MessageHandler, Owner, RoomName, AuthorizedUpdated, Users);
                _ ->
                    MessageHandler !
                        #direct_message{
                            socket = HostSocket,
                            message = #system{
                                message = io_lib:format("~s was alredy authorized", [Guest])
                            }
                        }
            end;
        {message, Sender, Message} ->
            chat_server_dynamodb:save_message(RoomName, Owner, #message{
                sender = Sender, message = Message
            }),
            MessageHandler !
                #broadcast_message{
                    clients = Users,
                    message =
                        #ok{message = io_lib:format("~s:~s", [Sender, Message])}
                };
        terminate ->
            MessageHandler !
                #broadcast_message{
                    clients = Users,
                    message =
                        #ok{message = "room destroyed by owner, you'll be kicked out"}
                },
            kick_everyone(Users),
            exit(normal)
    end,
    room_manager(MessageHandler, Owner, RoomName, Authorized, Users).

kick_everyone([]) ->
    ok;
kick_everyone([{User, _} | Users]) ->
    user_manager ! {update_user_room, {User, lobby}},
    kick_everyone(Users).
