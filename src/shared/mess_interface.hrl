%%% 
%%% Client messages to server

-record(login, {username}).



%%%
%%% Server responses
%%%

-record(ok, {info, message}).
-record(system, {message}).
-record(alert, {message}).
-record(error, {message}).

%%%
%%% Server states
%%% 

-record(user_state,{name, socket}).

