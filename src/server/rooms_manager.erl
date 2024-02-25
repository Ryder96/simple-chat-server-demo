-module(rooms_manager).

-export([start/1, manage/3]).

-include("../shared/mess_interface.hrl").

start(MessageHandler) ->
    chat_server_dynamodb:start(),
    FetchPublicRooms = chat_server_dynamodb:fetch_rooms(public),
    FetchPrivateRooms = chat_server_dynamodb:fetch_rooms(private),

    PublicRooms = start_room_manager(MessageHandler, FetchPublicRooms),
    PrivateRooms = start_room_manager(MessageHandler, FetchPrivateRooms),

    manage(MessageHandler, PublicRooms, PrivateRooms).

start_room_manager(_, []) ->
    [];
start_room_manager(MessageHandler, [H | T]) ->
    case H#room_state.type of
        public ->
            RoomMng_Pid = spawn(room_manager, room_manager, [
                MessageHandler, H#room_state.owner, H#room_state.name, []
            ]),
            NewRoom = H#room_state{room_pid = RoomMng_Pid},
            [NewRoom | start_room_manager(MessageHandler, T)];
        private ->
            RoomMng_Pid = spawn(pvt_room_manager, room_manager, [
                MessageHandler, H#room_state.owner, H#room_state.name, H#room_state.authorized, []
            ]),
            NewRoom = H#room_state{room_pid = RoomMng_Pid},
            [NewRoom | start_room_manager(MessageHandler, T)]
    end.

manage(MessageHandler, PublicRooms, PrivateRooms) ->
    receive
        {Socket, create, {Owner, Room}} when Room#room.type == public ->
            case chat_server_dynamodb:create_room(Room#room.name, Owner) of
                {ok, _} ->
                    %%% create new room
                    RoomMng_Pid = spawn(room_manager, room_manager, [
                        MessageHandler, Owner, Room#room.name, []
                    ]),
                    NewRoom = #room_state{
                        name = Room#room.name, owner = Owner, room_pid = RoomMng_Pid, type = public
                    },
                    Rooms2 = [NewRoom | PublicRooms],
                    MessageHandler !
                        #direct_message{
                            socket = Socket, message = #ok{message = "room created succefully"}
                        },
                    manage(MessageHandler, Rooms2, PrivateRooms);
                {error, _} ->
                    MessageHandler !
                        #direct_message{
                            socket = Socket,
                            message = #error{
                                message = io_lib:format("room: ~s already created", [
                                    Room#room.name
                                ])
                            }
                        }
            end;
        {Socket, create, {Owner, Room}} when Room#room.type == private ->
            case chat_server_dynamodb:create_room(Room#room.name, Owner, private) of
                {ok, _} ->
                    %%% create new room
                    RoomMng_Pid = spawn(pvt_room_manager, room_manager, [
                        MessageHandler, Owner, Room#room.name, [Owner], []
                    ]),
                    NewRoom = #room_state{
                        name = Room#room.name, owner = Owner, room_pid = RoomMng_Pid, type = private
                    },
                    Rooms2 = [NewRoom | PrivateRooms],
                    MessageHandler !
                        #direct_message{
                            socket = Socket, message = #ok{message = "room created succefully"}
                        },
                    user_manager ! {authorize, Owner, Room#room.name},
                    manage(MessageHandler, PublicRooms, Rooms2);
                {error, _} ->
                    MessageHandler !
                        #direct_message{
                            socket = Socket,
                            message = #ok{
                                message = io_lib:format("room [~s]: already created", [
                                    Room#room.name
                                ])
                            }
                        },
                    manage(MessageHandler, PublicRooms, PrivateRooms)
            end;
        {Socket, enter, {User, Room}} when Room#room.type == public ->
            case lists:keyfind(Room#room.name, #room_state.name, PublicRooms) of
                false ->
                    MessageHandler !
                        #direct_message{
                            socket = Socket, message = #error{message = "room does not exist"}
                        };
                FoundRoom ->
                    FoundRoom#room_state.room_pid ! {enter, {Socket, User}},
                    manage(MessageHandler, PublicRooms, PrivateRooms)
            end;
        {Socket, enter, {User, Room}} when Room#room.type == private ->
            case lists:keyfind(Room#room.name, #room_state.name, PrivateRooms) of
                false ->
                    MessageHandler !
                        #direct_message{
                            socket = Socket, message = #error{message = "room does not exist"}
                        };
                FoundRoom ->
                    FoundRoom#room_state.room_pid ! {enter, {Socket, User}}
            end;
        {Room, exit, User} when Room#room.type == public ->
            RoomState = lists:keyfind(Room#room.name, #room_state.name, PublicRooms),
            RoomState#room_state.room_pid ! {exit, User};
        {Room, exit, User} when Room#room.type == private ->
            RoomState = lists:keyfind(Room#room.name, #room_state.name, PrivateRooms),
            RoomState#room_state.room_pid ! {exit, User};
        {Socket, destroy, {Requester, Room}} when Room#room.type == public ->
            case lists:keyfind(Room#room.name, #room_state.name, PublicRooms) of
                false ->
                    MessageHandler !
                        #direct_message{
                            socket = Socket, message = #system{message = "room does not exits"}
                        };
                FoundRoom ->
                    if
                        FoundRoom#room_state.owner =:= Requester ->
                            unlink(Room#room_state.room_pid),
                            FoundRoom#room_state.room_pid ! terminate,
                            Rooms2 = lists:keydelete(Room#room.name, #room_state.name, PublicRooms),
                            MessageHandler !
                                #direct_message{
                                    socket = Socket, message = #ok{message = "room destroyed"}
                                },
                            manage(MessageHandler, Rooms2, PrivateRooms);
                        Room#room_state.owner /= Requester ->
                            MessageHandler !
                                #direct_message{
                                    socket = Socket,
                                    message = #system{message = "you are not the owner of the room"}
                                },
                            manage(MessageHandler, PublicRooms, PrivateRooms)
                    end
            end;
        {Socket, destroy, {Requester, Room}} when Room#room.type == private ->
            case lists:keyfind(Room#room.name, #room_state.name, PrivateRooms) of
                false ->
                    MessageHandler !
                        #direct_message{
                            socket = Socket, message = #system{message = "room does not exits"}
                        };
                FoundRoom ->
                    if
                        FoundRoom#room_state.owner =:= Requester ->
                            unlink(Room#room_state.room_pid),
                            FoundRoom#room_state.room_pid ! terminate,
                            Rooms2 = lists:keydelete(
                                Room#room.name, #room_state.name, PrivateRooms
                            ),
                            MessageHandler !
                                #direct_message{
                                    socket = Socket, message = #ok{message = "room destroyed"}
                                },
                            manage(MessageHandler, PublicRooms, Rooms2);
                        Room#room_state.owner /= Requester ->
                            MessageHandler !
                                #direct_message{
                                    socket = Socket,
                                    message = #system{message = "you are not the owner of the room"}
                                }
                    end
            end;
        {Room, message, {Sender, Message}} when Room#room.type == public ->
            case lists:keyfind(Room#room.name, #room_state.name, PublicRooms) of
                FoundRoom ->
                    FoundRoom#room_state.room_pid ! {message, Sender, Message}
            end,
            manage(MessageHandler, PublicRooms, PrivateRooms);
        {Room, message, {Sender, Message}} when Room#room.type == private ->
            case lists:keyfind(Room#room.name, #room_state.name, PrivateRooms) of
                FoundRoom ->
                    FoundRoom#room_state.room_pid ! {message, Sender, Message}
            end,
            manage(MessageHandler, PublicRooms, PrivateRooms);
        {Socket, Requester, list} ->
            case length(PublicRooms) of
                0 ->
                    MessageHandler !
                        #direct_message{
                            socket = Socket, message = #ok{message = "no rooms available"}
                        };
                N ->
                    Names = lists:map(
                        fun(Room) ->
                            Room#room_state.name
                        end,
                        PublicRooms
                    ),

                    Buffer = [io_lib:format("Room availables ~p:", [N]) | Names],
                    MessageHandler !
                        #direct_message{
                            socket = Socket, message = #ok{message = Buffer, info = print_list}
                        }
            end,

            case length(PrivateRooms) of
                0 ->
                    MessageHandler !
                        #direct_message{
                            socket = Socket, message = #ok{message = "no pvt rooms available"}
                        };
                PvtN ->
                    PvtNames = lists:map(
                        fun(Room) ->
                            case lists:member(Requester, Room#room_state.authorized) of
                                true ->
                                    Room#room_state.name;
                                false ->
                                    []
                            end
                        end,
                        PrivateRooms
                    ),

                    PvtBuffer = [io_lib:format("Private Room availables ~p:", [PvtN]) | PvtNames],
                    MessageHandler !
                        #direct_message{
                            socket = Socket, message = #ok{message = PvtBuffer, info = print_list}
                        }
            end;
        {Socket, invite, GuestSocket, Requester, Guest, RoomName} ->
            case lists:keyfind(RoomName, #room_state.name, PrivateRooms) of
                false ->
                    MessageHandler !
                        #direct_message{
                            socket = Socket, message = {#system{message = "room does not exits"}}
                        };
                Room ->
                    if
                        Room#room_state.owner =:= Requester ->
                            Room#room_state.room_pid ! {authorize, Socket, Guest},
                            user_manager ! {authorize, Guest, RoomName},
                            MessageHandler !
                                #direct_message{
                                    socket = Socket,
                                    message =
                                        #system{
                                            message = io_lib:format("user: ~s invited", [Guest])
                                        }
                                },
                            MessageHandler !
                                #direct_message{
                                    socket = GuestSocket,
                                    message =
                                        #system{
                                            message = io_lib:format("you can access room: ~s", [
                                                RoomName
                                            ])
                                        }
                                };
                        Room#room_state.owner /= Requester ->
                            MessageHandler !
                                #direct_message{
                                    socket = Socket,
                                    message =
                                        #system{
                                            message = "you are not the owner of the room"
                                        }
                                }
                    end
            end
    end,
    manage(MessageHandler, PublicRooms, PrivateRooms).
