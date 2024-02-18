-module(chat_server).

-export([start/0, server/1, loop/1, message_handler/0]).

-include("config/chat_config.hrl").
-include("shared/mess_interface.hrl").
%%%
%%% starts N sever that listen on the server port configured on settings
%%%
start() ->
    MessageHandlerPid = spawn(chat_server, message_handler, []),
    link(MessageHandlerPid),

    UserManagerPid = spawn(user_manager, user_manager, [MessageHandlerPid, []]),
    link(UserManagerPid),
    register(user_manager, UserManagerPid),

    case gen_tcp:listen(?server_port, [{active, false}, {packet, 2}, {mode, binary}]) of
        {ok, ListenSock} ->
            start_servers(?max_server_listener, ListenSock),
            {ok, Port} = inet:port(ListenSock),
            Port;
        {error, Reason} ->
            {error, Reason}
    end.

start_servers(0, _) ->
    ok;
start_servers(Num, LS) ->
    spawn(?MODULE, server, [LS]),
    start_servers(Num - 1, LS).

server(LS) ->
    case gen_tcp:accept(LS) of
        {ok, S} ->
            loop(S),
            server(LS)
    end.

%%%
%%% Listen client requests and manages if user crashs
%%%

loop(Socket) ->
    inet:setopts(Socket, [{active, true}]),
    receive
        {tcp, Socket, Data} ->
            process(Socket, Data),
            loop(Socket);
        {tcp_closed, Socket} ->
            io:format("Socket ~w closed [~w]~n", [Socket, self()]),
            user_manager ! {Socket, logout},
            ok;
        {tcp_error, Socket, econnaborted} ->
            io:format("Connection aborted by peer.~n"),
            user_manager ! {Socket, logout},
            ok;
        {tcp_error, Socket, Reason} ->
            io:format("Socket ~w closed due to error: ~p~n", [Socket, Reason]),
            user_manager ! {Socket, logout},
            ok
    end.

%%%
%%% Process the client request
%%%

process(Socket, Data) ->
    DecodedData = binary_to_term(Data),
    case DecodedData of
        #login{username = Name} ->
            user_manager ! {Socket, login, Name}
    end.



%%%
%%% Send messages to the client
%%%

message_handler() ->
    receive
        {Socket, Message} ->
            gen_tcp:send(Socket, term_to_binary(Message))
    end,
    message_handler().

