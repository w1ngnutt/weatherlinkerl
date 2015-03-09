-module(weatherlinkerl_app).

-behaviour(application).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    weatherlinkerl_sup:start_link().

stop(_State) ->
    ok.

%% ===================================================================
%% development purposed functions
%% ===================================================================

start() ->
    ok = application:start(ibrowse),   
    ok = application:start(weatherlinkerl).

%% start unit tests
-ifdef(TEST).

simple_test() ->
    ok = application:start(weatherlinkerl),
    ?assertNot(undefined == whereis(weatherlinkerl_sup)).

-endif.
