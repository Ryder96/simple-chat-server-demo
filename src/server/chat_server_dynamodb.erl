-module(chat_server_dynamodb).

-export([
    start/0,
    fetch_rooms/1,
    create_room/2, create_room/3,
    fetch_messages/2,
    save_message/3,
    authorize/3
]).

-include("../shared/mess_interface.hrl").
-include("../config/chat_config.hrl").

start() ->
    connect_to_dynamo().

connect_to_dynamo() ->
    DynamoHost = os:getenv("DYNAMODB_HOST"),
    DynamoPort = os:getenv("DYNAMODB_PORT"),
    
    case {DynamoHost, DynamoPort} of
    {false, _} -> 
        erlcloud_ddb2:configure(
            "12345",
            "secretpassword",
            ?dynamodb_host,
            ?dynamodb_port,
            "http://"
        );
    {Host, Port} when Host /= false, Port /= false ->
        {ParsedPort, _} = string:to_integer(Port),
        erlcloud_ddb2:configure(
            "12345",
            "secretpassword",
            Host,
            ParsedPort,
            "http://"
        )
end.

create_room(Name, Owner) ->
    create_room(Name, Owner, public).

create_room(Name, Owner, Type) when Type =:= public ->
    connect_to_dynamo(),

    erlcloud_ddb2:put_item(
        <<"Rooms">>,
        [
            {<<"Owner">>, {s, list_to_binary(Owner)}},
            {<<"Name">>, {s, list_to_binary(Name)}},
            {<<"Messages">>, {l, []}},
            {<<"Type">>, {s, atom_to_binary(Type)}}
        ],
        [
            {condition_expression, <<"attribute_not_exists(#name)">>},
            {expression_attribute_names, [{<<"#name">>, <<"Name">>}]}
        ]
    );
create_room(Name, Owner, Type) when Type =:= private ->
    connect_to_dynamo(),

    erlcloud_ddb2:put_item(
        <<"Rooms">>,
        [
            {<<"Owner">>, {s, list_to_binary(Owner)}},
            {<<"Name">>, {s, list_to_binary(Name)}},
            {<<"Messages">>, {l, []}},
            {<<"Type">>, {s, atom_to_binary(Type)}},
            {<<"Authorized">>, {l, [{s, list_to_binary(Owner)}]}}
        ],
        [
            {condition_expression, <<"attribute_not_exists(#name)">>},
            {expression_attribute_names, [{<<"#name">>, <<"Name">>}]}
        ]
    ).
save_message(Room, Owner, Data) ->
    connect_to_dynamo(),
    Time = os:system_time(second) * 1000,
    BinComplete = <<"SET Messages=list_append(Messages,:message)">>,
    case
        erlcloud_ddb2:update_item(
            <<"Rooms">>,
            [
                {<<"Name">>, {s, list_to_binary(Room)}},
                {<<"Owner">>, {s, list_to_binary(Owner)}}
            ],
            BinComplete,
            [
                {
                    expression_attribute_values, [
                        {<<":message">>,
                            {l, [
                                {m, [
                                    {<<"sender">>, list_to_binary(Data#message.sender)},
                                    {<<"timestamp">>, Time},
                                    {<<"body">>, list_to_binary(Data#message.message)}
                                ]}
                            ]}}
                    ]
                }
            ]
        )
    of
        {ok, _} -> ok;
        {error, Reason} -> io:format("~p~n", [Reason])
    end.

fetch_messages(Room, Owner) ->
    connect_to_dynamo(),
    case
        erlcloud_ddb2:get_item(
            <<"Rooms">>,
            [
                {<<"Name">>, list_to_binary(Room)},
                {<<"Owner">>, list_to_binary(Owner)}
            ],
            [
                {projection_expression, <<"Messages">>}
            ]
        )
    of
        {ok, Value} ->
            Messages = proplists:get_value(<<"Messages">>, Value),
            Parsed = parse_message_map(Messages),
            Sorted = lists:sort(
                fun(A, B) ->
                    A#message_ddb.timestamp > B#message_ddb.timestamp
                end,
                Parsed
            ),
            FirstNMessages = lists:sublist(Sorted, 1, 10),
            lists:reverse(FirstNMessages);
        Any ->
            io:format("fail~n~p,~n", [Any])
    end.

fetch_rooms(Type) ->
    RoomList = scan(Type),
    parse_response(RoomList).

scan(Type) ->
    connect_to_dynamo(),
    erlcloud_ddb2:scan(
        <<"Rooms">>,
        [
            {filter_expression, <<"#pvt = :private">>},
            {expression_attribute_names, [
                {<<"#pvt">>, <<"Type">>}
            ]},
            {expression_attribute_values, [
                {<<":private">>, atom_to_binary(Type)}
            ]}
        ]
    ).

authorize(RoomName, Owner, Guest) ->
    connect_to_dynamo(),
    BinComplete = <<"SET Authorized = list_append(Authorized, :guest)">>,
    erlcloud_ddb2:update_item(
        <<"Rooms">>,
        [
            {<<"Name">>, {s, list_to_binary(RoomName)}},
            {<<"Owner">>, {s, list_to_binary(Owner)}}
        ],
        BinComplete,
        [
            {
                expression_attribute_values, [
                    {<<":guest">>, {l, [{s, list_to_binary(Guest)}]}}
                ]
            }
        ]
    ).

parse_response({ok, Response}) when is_list(Response) ->
    Rooms = lists:map(
        fun(Item) ->
            Name = proplists:get_value(<<"Name">>, Item),
            Owner = proplists:get_value(<<"Owner">>, Item),
            Type = proplists:get_value(<<"Type">>, Item),
            Authorized = proplists:get_value(<<"Authorized">>, Item),
            #room_state{
                name = binary_to_list(Name),
                owner = binary_to_list(Owner),
                type = binary_to_atom(Type),
                authorized = parse_list(Authorized)
            }
        end,
        Response
    ),
    Rooms.

parse_list(undefined) ->
    undefined;
parse_list([H | T]) ->
    [binary_to_list(H) | parse_list(T)];
parse_list([]) ->
    [].

parse_message_map([]) ->
    [];
parse_message_map([H | T]) ->
    [parse_inner_item(H) | parse_message_map(T)].

parse_inner_item([{<<"body">>, Body}, {<<"sender">>, Sender}, {<<"timestamp">>, Timestamp}]) ->
    #message_ddb{
        body = binary_to_list(Body), sender = binary_to_list(Sender), timestamp = Timestamp
    }.
