# Simple Chat Server Demo

## An OTP Application

### Build

```bash
rebar3 compile
```

### Server

Open a shell and start the server by executing `rebar3 shell`, then call `chat_server:start()`.

### Client

Start a client on any shell, including the server shell, but only one client per shell is allowed. To initiate the client, use `chat_client:start()`.

### User Interface

In the client shell, users can interact with the server using the `user_interface` module. Before any action, users must log in by calling `user_interface:login("username")`.

Users can create public and private rooms and destroy them only if they are the room owner. Users can message only if they are inside a room or whisper to another user.

#### User Commands:

- `user_interface:login("username")`: Log in to the server.
- `user_interface:logout()`: Log out from the server.
- `user_interface:create_room("room name")`: Create a public room visible to all users.
- `user_interface:create_pv_room("room name")`: Create a private room visible only to authorized users.
- `user_interface:destroy_room("room name")`: Delete a public room.
- `user_interface:destroy_pvt_room("room name")`: Delete a private room.
- `user_interface:enter_room("room name")`: Enter a room if it exists.
- `user_interface:exit_room()`: Exit the current room.
- `user_interface:list_rooms()`: View all available rooms. Public rooms are displayed to everyone, but private rooms are only visible to those with access.
- `user_interface:invite_pvt_room("User Name", "Room")`: Invite a user to a private room.
- `user_interface:send_message("Message")`: Send a message inside the current room.
- `user_interface:whisper("Dst Name", "Message")`: Whisper to a user privately.

### DynamoDB

DynamoDB is used to save the rooms created by the users and the messages sent.