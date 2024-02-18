-module(chat_client).

-export([start/0, client/0, client/1, client/2]).

-include("../config/chat_config.hrl").
-include("../shared/mess_interface.hrl").

%%%
%%% Starts the chat client
%%% The first time or if the server crashed it will spawn the client process
%%% After it will simple say 'already_started'
%%%
start() ->
    case whereis(mess_client) of
        undefined ->
            Client_Pid = spawn(chat_client, client, []),
            link(Client_Pid),
            register(mess_client, Client_Pid);
        _ ->
            already_started
    end.

%%%
%%% Connects the client to the server socket
%%%
client() ->
    case
        gen_tcp:connect(?server_host, ?server_port, [{active, false}, {packet, 2}, {mode, binary}])
    of
        {ok, Sock} ->
            io:format("connected to server~n", []),
            client(Sock);
        {error, Reason} ->
            io:format("server refused connection: ~p~n", [Reason])
    end.

%%%
%%% Client while the user is not logged it will wait and blocks any other operation
%%%
client(Socket) ->
    receive
        #login{username = Name} ->
            gen_tcp:send(Socket, term_to_binary(#login{username = Name})),
            case gen_tcp:recv(Socket, 0) of
                {ok, Binary} ->
                    ResponseSucc = receive_message(Binary),
                    case ResponseSucc of
                        true ->
                            spawn_link(fun() -> chat_listen(Socket) end),
                            client(Socket, Name);
                        false ->
                            io:format("failed to logging username already used~n", []),
                            client(Socket)
                    end;
                {error, Any} ->
                    socket_error_handler(Socket, {error, Any}),
                    exit(normal)
            end;
        _ ->
            io:format("you must login before!~n", [])
    end,
    client(Socket).

%%%
%%% Client execs commands given by the user to chat
%%%
client(Socket, Name) ->
    receive
        logout ->
            io:format("disconnected~n", []),
            exit(normal);
        %%% message handler
        {message, Message} ->
            gen_tcp:send(Socket, term_to_binary(#message{sender = Name, body = Message}));
        %%% room management
        {create_room, RoomName} ->
            gen_tcp:send(
                Socket, term_to_binary(#create_room{requester = Name, room_name = RoomName})
            );
        {destroy_room, RoomName} ->
            gen_tcp:send(
                Socket, term_to_binary(#destroy_room{requester = Name, room_name = RoomName})
            );
        {enter_room, RoomName} ->
            gen_tcp:send(
                Socket, term_to_binary(#enter_room{requester = Name, room_name = RoomName})
            );
        exit_room ->
            gen_tcp:send(Socket, term_to_binary(#exit_room{user = Name}));
        list_rooms ->
            gen_tcp:send(Socket, term_to_binary(#list_rooms{requester = Name}));
        close_client ->
            exit(normal)
    end,
    client(Socket, Name).

%%%
%%% Handles the different types of response
%%%

receive_message(Binary) ->
    Message = binary_to_term(Binary),
    case Message of
        #ok{info = logged, message = _} ->
            io:format("logged in~n", []),
            true;
        #ok{info = print_list, message = MsgBinary} ->
            print_list(MsgBinary),
            true;
        #ok{info = _, message = MsgBinary} ->
            Msg = lists:flatten(MsgBinary),
            io:format("~s~n", [Msg]),
            true;
        #system{message = MsgBinary} ->
            Msg = lists:flatten(MsgBinary),
            io:format("~p~n", [Msg]),
            true;
        #alert{message = MsgBinary} ->
            Msg = lists:flatten(MsgBinary),
            io:format("~p~n", [Msg]),
            true;
        #error{message = MsgBinary} ->
            io:format("~p~n", [MsgBinary]),
            false
    end.

%%%
%%% Chat listeners awaits server response
%%%

chat_listen(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Binary} ->
            receive_message(Binary),
            chat_listen(Socket);
        {error, Any} ->
            socket_error_handler(Socket, {error, Any}),
            exit(normal)
    end,
    chat_listen(Socket).

%%%
%%% Manage socket errors
%%%

socket_error_handler(Socket, ErrorValue) ->
    case ErrorValue of
        {error, closed} ->
            io:format("Server crashed disconnecting~n"),
            gen_tcp:close(Socket);
        {error, econnaborted} ->
            io:format("Connection aborted by peer.~n"),
            io:format("Try loggin again~n");
        {error, enotconn} ->
            io:format("Server crashed disconnecting~n")
    end,
    gen_tcp:close(Socket).

%%%
%%% Utilities
%%%
print_list([]) ->
    ok;
print_list([H | T]) ->
    Item = lists:flatten(H),
    io:format("~p~n", [Item]),
    print_list(T).
