-module(room_manager).

-export([room_manager/4]).

-include("../shared/mess_interface.hrl").

room_manager(MessageHandler, Owner, RoomName, Users) ->
    receive
        {enter, {Socket, User}} ->
            case lists:keyfind(User, 1, Users) of
                false ->
                    Users2 = [{User, Socket} | Users],
                    user_manager !
                        #update_user_room{
                            user = User, new_room = #room{name = RoomName, type = public}
                        },
                    MessageHandler !
                        #broadcast_message{
                            clients = Users,
                            message =
                                #system{message = io_lib:format("~p enter the room", [User])}
                        },
                    MessageHandler !
                        #direct_message{
                            socket = Socket, message = #ok{message = "you entered the room"}
                        },
                    room_manager(MessageHandler, Owner, RoomName, Users2);
                _ ->
                    MessageHandler !
                        #direct_message{
                            socket = Socket, message = #error{message = "already in room"}
                        },
                    room_manager(MessageHandler, Owner, RoomName, Users)
            end;
        {exit, User} ->
            Users2 = lists:keydelete(User, 1, Users),
            MessageHandler !
                #broadcast_message{
                    clients = Users,
                    message =
                        #system{message = io_lib:format("~p exit the room", [User])}
                },
            room_manager(MessageHandler, Owner, RoomName, Users2);
        {message, Sender, Message} ->
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
    room_manager(MessageHandler, Owner, RoomName, Users).

kick_everyone([]) ->
    ok;
kick_everyone([{User, _} | Users]) ->
    user_manager ! {update_user_room, {User, lobby}},
    kick_everyone(Users).
