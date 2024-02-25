%%%-------------------------------------------------------------------
%% @doc chat public API
%% @end
%%%-------------------------------------------------------------------

-module(simple_chat_server_demo).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    application:ensure_all_started(erlcloud),
    simple_chat_server_demo_sup:start_link().

stop(_State) ->
    ok.
