-module(weatherlink_archive_client).

-export([
        calc_timestamp/1,
        fetch_headers/3,
        fetch_archive/3,
        open_sample/0,
        close_sample/1,
        read_packet/1,
        print_packets/1,
        type/1,
        packet_to_record/1,
        fetch_archive_with_minutes/3
    ]).

-include("weatherlink.hrl").

-define(BASE_URL, "http://weatherlink.com/webdl.php?timestamp=~B&user=~s&pass=~s&action=~s").
-define(DMP_PKT_LENGTH, 52).

fetch_headers(User, Pass, Timestamp) ->
    Url = lists:flatten(io_lib:format(?BASE_URL, [Timestamp, User, Pass, "headers"])),
    case httpc:request(Url) of
        {ok, {_Status, _Headers, Body}} ->
            Lines = string:tokens(Body, "\n"),
            PL = lists:map(fun(X)-> [A,B]=string:tokens(X, "="), {A,B} end, Lines),
            #archive_headers{
                model=element(1, string:to_integer(proplists:get_value("Model", PL, "0"))),
                num_records=element(1, string:to_integer(proplists:get_value("Records", PL, "0"))),
                max_records=element(1, string:to_integer(proplists:get_value("MaxRecords", PL, "0"))),
                archive_interval=element(1, string:to_integer(proplists:get_value("ArchiveInt", PL, "0"))),
                console_version=proplists:get_value("ConsoleVer", PL, "Unknown"),
                vantage_tx=element(1, string:to_integer(proplists:get_value("VantageTX", PL, "0")))
              };
        {error, Error} ->
            {error, Error}
    end.

fetch_archive(User, Pass, Timestamp) ->
    Url = io_lib:format(?BASE_URL, [Timestamp, User, Pass, "data"]),
    case httpc:request(get, {Url, []}, [], [{body_format, binary}]) of
        {ok, {_Status, _Headers, Body}} ->
            parse_archive(Body);
        {error, Error} ->
            {error, Error}
    end.

fetch_archive_with_minutes(User, Pass, MinutesInThePast) ->
    DT = calendar:local_time(),
    Now = calendar:datetime_to_gregorian_seconds(DT),
    Past = calendar:gregorian_seconds_to_datetime(Now-60*MinutesInThePast),
    fetch_archive(User, Pass, calc_timestamp(Past)).

%% @doc takes a datetime() and returns a weatherlink timestamp
calc_timestamp({{Yr, Month, Day}, {Hr, Min, _Sec}}) ->
    Date = Day + (Month bsl 5) + ((Yr-2000) bsl 9),
    Time = Hr*100 + Min,
    (Date bsl 16) bor Time.

open_sample() ->
    File = code:priv_dir(?STR(?APP)) ++ "/sample-loop.dat",
    {ok, IO} = file:open(lists:flatten(File), [read,binary]),
    IO.

close_sample(IO) ->
    file:close(IO).

read_packet(IO) ->
    BadPkt = list_to_binary(lists:duplicate(?DMP_PKT_LENGTH, 255)),
    case file:read(IO, ?DMP_PKT_LENGTH) of
        {ok, BadPkt} ->
            done;
        {ok, Pkt} ->
            <<B:8, B1:8, B2:8, B3:8>> = binary:part(Pkt, 0, 4),
            io:format("~B, ~B, ~B, ~B~n", [B, B1, B2, B3]),
            Pkt;
        eof ->
            done;
        {error, _Error} ->
            done
    end.

parse_archive(A) ->
    parse_archive(A, []).

parse_archive(<<>>, List) ->
    List;
parse_archive(Bin, List) when length(Bin) < ?DMP_PKT_LENGTH ->
    List;
parse_archive(<<A:52/binary, Rest/binary>>, List) ->
    BadPkt = list_to_binary(lists:duplicate(?DMP_PKT_LENGTH, 255)),
    if
        A =:= BadPkt ->
            List;
        true ->
            R = packet_to_record(A),
            parse_archive(Rest, List ++ [R])
    end.

format_packet(P) ->
    Type = type(P),
    {Y, M, D} = date(P),
    {Hr, Min} = time(P),
    Temp = outside_temp(P),
    HighTemp = high_temp(P),
    LowTemp = low_temp(P),
    Rain = rainfall(P),
    % io:format("~p\n", [P]),
    io:format("Time: ~w-~w-~w ~w:~w - Temp: ~w (~w,~w), Rain: ~w, Type: ~w\n",
        [Y, M, D, Hr, Min, Temp, LowTemp, HighTemp, Rain, Type]).

print_packets(IO, done) ->
    close_sample(IO),
    ok;
print_packets(IO, P) ->
    format_packet(P),
    print_packets(IO, read_packet(IO)).

print_packets(IO) ->
    print_packets(IO, read_packet(IO)).

type(Pkt) ->
    Type = binary:at(Pkt,42),
    if
        Type =:= 0 ->
            rev_b;
        Type =:= 255 ->
            rev_a;
        true ->
            unknown
    end.

%% @doc read a little-endian short from the LOOP archive
short_binary(Pkt, Start) ->
    <<B1:8, B2:8>> = binary:part(Pkt, Start, 2),
    <<B2, B1>>.

short(Pkt, Start) ->
    <<I:16/integer>> = short_binary(Pkt, Start),
    I.

char(Pkt, Start) ->
    <<I:8/integer>> = binary:part(Pkt, Start, 1),
    I.

date(Pkt) ->
    <<Y:7,M:4,D:5>> = short_binary(Pkt, 0),
    {2000+Y, M, D}.

time(Pkt) ->
    T = short(Pkt, 2),
    { T div 100, T rem 100 }.

outside_temp(Pkt) ->
    short(Pkt, 4) / 10.

high_temp(Pkt) ->
    short(Pkt, 6) / 10.

low_temp(Pkt) ->
    short(Pkt, 8) / 10.

rainfall(Pkt) ->
    short(Pkt, 10) * 0.01.

%% @doc measured in clicks per hour
high_rain_rate(Pkt) ->
    short(Pkt, 12).

%% @doc measured in Hg / 1000
barometer(Pkt) ->
    short(Pkt, 14).

%% @doc measured in Watts / m^2
solar_radiation(Pkt) ->
    short(Pkt, 16).

num_wind_samples(Pkt) ->
    short(Pkt, 18).

inside_temp(Pkt) ->
    short(Pkt, 20)/10.

inside_humidity(Pkt) ->
    char(Pkt, 22).

outside_humidity(Pkt) ->
    char(Pkt, 23).

avg_wind_speed(Pkt) ->
    char(Pkt, 24).

high_wind_speed(Pkt) ->
    char(Pkt, 25).

high_wind_speed_dir(Pkt) ->
    %% TODO: convert ints to respective N, NNE, etc
    char(Pkt, 26).

pervailing_wind_dir(Pkt) ->
    %% TODO: convert ints to respective N, NNE, etc
    char(Pkt, 27).

avg_uv_index(Pkt) ->
    char(Pkt, 28) * 10.

%% @doc ET accumulated over last hour. Units: in / 1000
et(Pkt) ->
    char(Pkt, 29).

%% @doc Highest Solar Rad value over the archive period. Units: Watts / m^2
high_solar_radiation(Pkt) ->
    short(Pkt, 30).

%% @doc Highest Uv index over archive period. Units: Watts / m^2
uv_high_index(Pkt) ->
    char(Pkt, 32).

forecast_rule(Pkt) ->
    char(Pkt, 33).

leaf_temp(Pkt) ->
    T1 = char(Pkt, 34) - 90,
    T2 = char(Pkt, 35) - 90,
    {T1, T2}.

leaf_wetness(Pkt) ->
    W1 = char(Pkt, 36),
    W2 = char(Pkt, 37),
    {W1, W2}.

soil_temps(Pkt) ->
    <<T1:8, T2:8, T3:8, T4:8>> = binary:part(Pkt, 38, 4),
    {T1-90, T2-90, T3-90, T4-90}.

extra_humidities(Pkt) ->
    <<H1:8, H2:8>> = binary:part(Pkt, 43, 2),
    {H1, H2}.

extra_temps(Pkt) ->
    <<T1:8, T2:8, T3:8>> = binary:part(Pkt, 45, 3),
    {T1-90, T2-90, T3-90}.

soil_moistures(Pkt) ->
    <<M1:8, M2:8, M3:8, M4:8>> = binary:part(Pkt, 48, 4),
    {M1, M2, M3, M4}.

packet_to_record(Pkt) ->
    #archive{
       date=date(Pkt),
       time=time(Pkt),
       outside_temp=outside_temp(Pkt),
       high_temp=high_temp(Pkt),
       low_temp=low_temp(Pkt),
       rainfall=rainfall(Pkt),
       high_rain_rate=high_rain_rate(Pkt),
       barometer=barometer(Pkt),
       solar_radiation=solar_radiation(Pkt),
       num_wind_samples=num_wind_samples(Pkt),
       inside_temp=inside_temp(Pkt),
       inside_humidity=inside_humidity(Pkt),
       outside_humidity=outside_humidity(Pkt),
       avg_wind_speed=avg_wind_speed(Pkt),
       high_wind_speed=high_wind_speed(Pkt),
       high_wind_speed_dir=high_wind_speed_dir(Pkt),
       pervailing_wind_dir=pervailing_wind_dir(Pkt),
       avg_uv_index=avg_uv_index(Pkt),
       et=et(Pkt),
       high_solar_radiation=high_solar_radiation(Pkt),
       uv_high_index=uv_high_index(Pkt),
       forecast_rule=forecast_rule(Pkt),
       leaf_temp=leaf_temp(Pkt),
       leaf_wetness=leaf_wetness(Pkt),
       soil_temps=soil_temps(Pkt),
       extra_humidities=extra_humidities(Pkt),
       extra_temps=extra_temps(Pkt),
       soil_moistures=soil_moistures(Pkt)
      }.
