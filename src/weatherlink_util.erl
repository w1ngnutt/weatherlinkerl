-module(weatherlink_util).

-export([
        to_str/1,
        degrees_to_dir/1,
        readable_wind_direction/1
    ]).

-include("weatherlink.hrl").

to_str(S) ->
    if
        is_list(S) ->
            S;
        is_atom(S) ->
            atom_to_list(S);
        is_binary(S) ->
            unicode:characters_to_list(S);
        is_integer(S) ->
            integer_to_list(S);
        is_float(S) ->
            float_to_list(S);
        true ->
            {error, badarg}
    end.

degrees_to_dir(D) ->
    if
        D =< 11.25 orelse D > 348.75 ->
            "N";
        D =< 33.75 ->
            "NNE";
        D =< 56.25 ->
            "NE";
        D =< 78.75 ->
            "ENE";
        D =< 101.25 ->
            "E";
        D =< 123.75 ->
            "ESE";
        D =< 146.25 ->
            "SE";
        D =< 168.75 ->
            "SSE";
        D =< 191.25 ->
            "S";
        D =< 213.75 ->
            "SSW";
        D =< 236.25 ->
            "SW";
        D =< 258.75 ->
            "WSW";
        D =< 281.25 ->
            "W";
        D =< 303.75 ->
            "WNW";
        D =< 326.75 ->
            "NW";
        D =< 348.75 ->
            "NNW";
        true ->
            "UNKNWON"
    end.

readable_wind_direction(Dir) ->
    proplists:get_value(Dir, [
            {"N", "North"},
            {"NNE", "North North East"},
            {"NE", "North East"},
            {"ENE", "East North East"},
            {"E", "East"},
            {"ESE", "East South East"},
            {"SE", "South East"},
            {"SSE", "South South East"},
            {"S", "South"},
            {"SSW", "South South West"},
            {"SW", "South West"},
            {"WSW", "West South West"},
            {"W", "West"},
            {"WNW", "West North West"},
            {"NW", "North West"},
            {"NNW", "North North West"}
        ], "unknown"). 

