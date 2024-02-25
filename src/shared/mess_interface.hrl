%%%
%%% Client messages to server

-record(login, {username}).
-record(create_room, {requester, room}).
-record(destroy_room, {requester, room}).
-record(enter_room, {requester, room}).
-record(list_rooms, {requester}).
-record(exit_room, {user, type}).
-record(message, {sender, message}).
-record(whisper, {sender, dst, message}).
-record(invite, {host, guest, room_name}).
-record(room, {name, type}).

%%%
%%% Server responses
%%%

-record(ok, {info, message}).
-record(system, {message}).
-record(alert, {message}).
-record(error, {message}).

-record(direct_message, {socket, message}).
-record(broadcast_message, {clients, message}).

%%%
%%% Server states
%%%

-record(user_state, {name, socket, room, room_type, pvt_rooms}).
-record(room_state, {name, owner, room_pid, type, authorized}).

%%%
%%% User manager records
%%%

-record(update_user_room, {user, new_room, room_type}).


%%%
%%% Messages saved on dynamodb
%%%

-record(message_ddb, {sender, body, timestamp}).
