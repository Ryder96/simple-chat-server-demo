%%% 
%%% Client messages to server

-record(login, {username}).
-record(create_room, {requester, room_name}).
-record(destroy_room, {requester, room_name}).
-record(enter_room, {requester, room_name}).
-record(list_rooms, {requester}).
-record(exit_room, {user}).
-record(message, {sender, body}).

%%%
%%% Server responses
%%%

-record(ok, {info, message}).
-record(system, {message}).
-record(alert, {message}).
-record(error, {message}).

-record(direct_message,{socket,message}).
-record(broadcast_message,{clients, message}).

%%%
%%% Server states
%%% 

-record(user_state,{name, socket, room}).
-record(room_state,{name, owner, room_pid}).

%%%
%%% User manager records
%%%

-record(update_user_room,{user, new_room}).
