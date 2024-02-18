-module(rooms_manager).

-export([manage/2]).

-include("../shared/mess_interface.hrl").

manage(MessageHandler, Rooms) ->
    receive
        {Socket, create, {Owner, RoomName}} ->
            case lists:keyfind(RoomName, #room_state.name, Rooms) of
                false ->
                    %%% create new room
                    RoomMng_Pid = spawn(room_manager, manage, [
                        MessageHandler, Owner, RoomName, []
                    ]),
                    NewRoom = #room_state{
                        name = RoomName, owner = Owner, room_pid = RoomMng_Pid
                    },
                    Rooms2 = [NewRoom | Rooms],
                    MessageHandler !
                        #direct_message{
                            socket = Socket, message = #ok{message = "room created succefully"}
                        },
                    manage(MessageHandler, Rooms2);
                Room ->
                    MessageHandler !
                        #direct_message{
                            socket = Socket,
                            message = #error{
                                message = io_lib:format("room: ~s already created", [
                                    Room#room_state.name
                                ])
                            }
                        }
            end;
        {Socket, enter, {User, RoomName}} ->
            case lists:keyfind(RoomName, #room_state.name, Rooms) of
                false ->
                    MessageHandler !
                        #direct_message{
                            socket = Socket, message = #error{message = "room does not exist"}
                        };
                Room ->
                    Room#room_state.room_pid ! {enter, {Socket, User}},
                    manage(MessageHandler, Rooms)
            end;
        {RoomName, exit, User} ->
            RoomState = lists:keyfind(RoomName, #room_state.name, Rooms),
            RoomState#room_state.room_pid ! {exit, User};
        {Socket, destroy, {Requester, RoomName}} ->
            case lists:keyfind(RoomName, #room_state.name, Rooms) of
                false ->
                    MessageHandler !
                        #direct_message{
                            socket = Socket, message = #error{message = "room does not exits"}
                        };
                Room ->
                    if
                        Room#room_state.owner =:= Requester ->
                            unlink(Room#room_state.room_pid),
                            Room#room_state.room_pid ! terminate,
                            Rooms2 = lists:keydelete(RoomName, #room_state.name, Rooms),
                            MessageHandler !
                                #direct_message{
                                    socket = Socket, message = #ok{message = "room destroyed"}
                                },
                            manage(MessageHandler, Rooms2);
                        Room#room_state.owner /= Requester ->
                            MessageHandler !
                                #direct_message{
                                    socket = Socket,
                                    message = #system{message = "you are not the owner of the room"}
                                },
                            manage(MessageHandler, Rooms)
                    end
            end;
        {RoomName, message, {Sender, Message}} ->
            case lists:keyfind(RoomName, #room_state.name, Rooms) of
                Room ->
                    Room#room_state.room_pid ! {message, Sender, Message}
            end,
            manage(MessageHandler, Rooms);
        {Socket, list} ->
            case length(Rooms) of
                0 ->
                    MessageHandler !
                        #direct_message{
                            socket = Socket, message = #ok{message = "no rooms available"}
                        };
                N ->
                    Names = lists:map(
                        fun(Room) ->
                            io_lib:format("~w", [Room#room_state.name])
                        end,
                        Rooms
                    ),

                    Buffer = [io_lib:format("Room availables ~p:", [N]) | Names],
                    MessageHandler !
                        #direct_message{
                            socket = Socket, message = #ok{message = Buffer, info = print_list}
                        }
            end,
            manage(MessageHandler, Rooms)
    end,
    manage(MessageHandler, Rooms).
