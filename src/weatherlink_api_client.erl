-module(weatherlink_api_client).

-export([
        fetch_noaa_json/3,
        open_sample/0,
        close_sample/1,
        packet_to_record/1
    ]).

-include("weatherlink.hrl").

-define(BASE_URL, "https://api.weatherlink.com/v1/NoaaExt.json?user=~s&pass=~s&apiToken=~s").

fetch_noaa_json(User, Pass, ApiToken) ->
    Url = io_lib:format(?BASE_URL, [User, Pass, ApiToken]),
    case httpc:request(get, {Url, []}, [], [{body_format, binary}]) of
        {ok, {_Status, _Headers, Body}} ->
            {struct, Json} = mochijson2:decode(Body),
            packet_to_record(Json);
        {error, Error} ->
            {error, Error}
    end.

open_sample() ->
    File = code:priv_dir(?STR(?APP)) ++ "/sample.json",
    file:read_file(File).

close_sample(IO) ->
    file:close(IO).

dco(J) ->
    {struct, DCO} = proplists:get_value(<<"davis_current_observation">>, J),
    DCO.

num(undefined) ->
    0.0;
num(Bin) ->
    N = binary_to_list(Bin),
    case string:to_float(N) of
        {error,no_float} -> list_to_integer(N);
        {F,_Rest} -> F
    end.

datetime(J) ->
    S = proplists:get_value(<<"observation_time_rfc822">>, J),
    datetime:datetime_decode(S).

date({D, _T}) ->
    D.

time({_D, {H, M, _S}}) ->
    {H, M}.

outside_temp(J) ->
    num(proplists:get_value(<<"temp_f">>, J)).

high_temp(J) ->
    num(proplists:get_value(<<"temp_day_high_f">>, dco(J))).

low_temp(J) ->
    num(proplists:get_value(<<"temp_day_low_f">>, dco(J))).

rainfall(J) ->
    num(proplists:get_value(<<"rain_day_in">>, dco(J))).

high_rain_rate(J) ->
    num(proplists:get_value(<<"rain_rate_day_high_in_per_hr">>, dco(J))).

% pressure(J) ->
%    num(proplists:get_value(<<"pressure_mb">>, J)).

solar_radiation(_J) ->
    % TODO: see if this still exists
    undefined.

num_wind_samples(_J) ->
    % TODO
    undefined.

inside_temp(_J) ->
    % TODO
    undefined.

inside_humidity(_J) ->
    undefined.

outside_humidity(J) ->
    num(proplists:get_value(<<"relative_humidity">>, J)).

avg_wind_speed(J) ->
    num(proplists:get_value(<<"wind_ten_min_avg_mph">>, dco(J))).

high_wind_speed(J) ->
    num(proplists:get_value(<<"wind_day_high_mph">>, dco(J))).

high_wind_speed_dir(_J) ->
    % TODO
    undefined.

pervailing_wind_dir(J) ->
    proplists:get_value(<<"wind_dir">>, J).

avg_uv_index(_J) ->
    % TODO
    undefined.

et(_J) ->
    undefined.

high_solar_radiation(_J) ->
    undefined.

uv_high_index(_J) ->
    undefined.

forecast_rule(_J) ->
    undefined.

leaf_temp(_J) ->
    undefined.

leaf_wetness(_J) ->
    undefined.

soil_temps(_J) ->
    undefined.

extra_humidities(J) ->
    {num(proplists:get_value(<<"relative_humidity_1">>, dco(J))),
     num(proplists:get_value(<<"relative_humidity_2">>, dco(J)))}.

extra_temps(J) ->
    {num(proplists:get_value(<<"temp_extra_1">>, dco(J))),
     num(proplists:get_value(<<"temp_extra_2">>, dco(J))),
     num(proplists:get_value(<<"temp_extra_3">>, dco(J)))}.

soil_moistures(_J) ->
    {0, 0, 0}.

packet_to_record(J) ->
    #archive{
        date=date(datetime(J)),
        time=time(datetime(J)),
        outside_temp=outside_temp(J),
        high_temp=high_temp(J),
        low_temp=low_temp(J),
        rainfall=rainfall(J),
        high_rain_rate=high_rain_rate(J),
        % pressure=pressure(J),
        solar_radiation=solar_radiation(J),
        num_wind_samples=num_wind_samples(J),
        inside_temp=inside_temp(J),
        inside_humidity=inside_humidity(J),
        outside_humidity=outside_humidity(J),
        avg_wind_speed=avg_wind_speed(J),
        high_wind_speed=high_wind_speed(J),
        high_wind_speed_dir=high_wind_speed_dir(J),
        pervailing_wind_dir=pervailing_wind_dir(J),
        avg_uv_index=avg_uv_index(J),
        et=et(J),
        high_solar_radiation=high_solar_radiation(J),
        uv_high_index=uv_high_index(J),
        forecast_rule=forecast_rule(J),
        leaf_temp=leaf_temp(J),
        leaf_wetness=leaf_wetness(J),
        soil_temps=soil_temps(J),
        extra_humidities=extra_humidities(J),
        extra_temps=extra_temps(J),
        soil_moistures=soil_moistures(J)
    }.

