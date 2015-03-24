-module(weatherlink_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    weatherlink_sup:start_link().

stop(_State) ->
    ok.

%% ===================================================================
%% development purposed functions
%% ===================================================================

start() ->
    OK = application:start(inets),
    OK = application:start(weatherlink),
    OK.

%% start unit tests
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

simple_test() ->
    ok = start(),
    ?assertNot(undefined == whereis(weatherlink_sup)).

-endif.
