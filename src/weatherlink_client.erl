-module(weatherlink_client).

-export([
        headers/1,
        archive/1,
        current_summary/0,
        current_summary/1,
        active_alarms/1,
        temp/1,
        rain/1,
        wind/1,
        parse_body/1,
        row/2,
        load_sample/0,
        load_sample2/0
    ]).

-include("weatherlink.hrl").

-define(sample1, lists:flatten(code:priv_dir(?APP) ++ "/summary-sample.html")).
-define(sample2, lists:flatten(code:priv_dir(?APP) ++ "/summary-sample-no-alarm.html")).

-define(weather_table_xpath, "//body/div/div/table/tr[4]/td/table/tr[1]/td/table/tr[~b]/td/text()").
-define(weather_table_row(X), lists:flatten(io_lib:format(?weather_table_xpath, [X]))).
-define(station_url(User), lists:flatten( "http://www.weatherlink.com/user/" ++ User ++ "/index.php?view=summary&headers=1") ).


%% STATION SUMMARY 
% Name | Current | Today's Highs | Today's Lows
%      |         | Value | Time  | Value | Time
% outside temp  //body/div/div/table/tr[4]/td/table/tr[1]/td/table/tr[8]/td/text()
% outside humid //body/div/div/table/tr[4]/td/table/tr[1]/td/table/tr[9]/td/text()
% inside temp   //body/div/div/table/tr[4]/td/table/tr[1]/td/table/tr[11]/td/text()
% inside humid  //body/div/div/table/tr[4]/td/table/tr[1]/td/table/tr[12]/td/text()
% head index    //body/div/div/table/tr[4]/td/table/tr[1]/td/table/tr[14]/td/text()
% wind chill    //body/div/div/table/tr[4]/td/table/tr[1]/td/table/tr[15]/td/text()
% dew point     //body/div/div/table/tr[4]/td/table/tr[1]/td/table/tr[16]/td/text()
% barometer     //body/div/div/table/tr[4]/td/table/tr[1]/td/table/tr[18]/td/text()
% bar trend     //body/div/div/table/tr[4]/td/table/tr[1]/td/table/tr[19]/td/text()
% wind speed    //body/div/div/table/tr[4]/td/table/tr[1]/td/table/tr[21]/td/text()
% wind dir      //body/div/div/table/tr[4]/td/table/tr[1]/td/table/tr[22]/td/text()
% hrly forecast //body/div/div/table/tr[4]/td/table/tr[1]/td/table/tr[24]/td/text()
-define(temp_row,          8).
-define(humid_row,         9).
-define(heat_index_row,   11).
-define(wind_chill_row,   12).
-define(dew_point_row,    13).
-define(barometer_row,    15).
-define(bar_trend_row,    16).
-define(wind_speed_row,   18).
-define(wind_speed_dir,   19).
-define(hourly_fcast_row, 21).

%% WIND
% Name  |  2-min  |  10-min
% avg wind      //body/div/div/table/tr[4]/td/table/tr[1]/td/table/tr[29]/td/text()
% wind gust     //body/div/div/table/tr[4]/td/table/tr[1]/td/table/tr[30]/td/text()
-define(wind_avg_row,  26).
-define(wind_gust_row, 27).


%% RAIN
% Name | Rate | Day | Storm | Month | Year
% rain       //body/div/div/table/tr[4]/td/table/tr[1]/td/table/tr[35]/td/text()
% last hr    //body/div/div/table/tr[4]/td/table/tr[1]/td/table/tr[36]/td/text()
-define(rain_row,         32).
-define(rain_last_hr_row, 33).


%% ALARMS
% for these it's probably best to just regex match the contents of the list of
% string returned by the below xpath

% Inside | Outside | Rain | Extra
% alarms    //body/div/div/table/tr[4]/td/table/tr[1]/td/table/tr[40]/td/text()
-define(alarms_row, 40).



%% Make a HTTP request to weatherlink.com for archive headers
headers(_Date) ->
    ok.

%% Make a HTTP request to weatehrlink.com for archive records since Date
archive(_Date) ->
    ok.

%% Make a HTTP request to weatherlink.com for the current station weather summary
current_summary() ->
    case application:get_env(?APP, station_name) of
        undefiend ->
            {error, no_station_name};
        Station ->
            current_summary(Station)
    end.

current_summary(Station) ->
    Url = ?station_url(Station),
    case httpc:request(Url) of
        {ok, {_Status, _Headers, Body}} ->
            html_to_record(Body);
        {error, Reason} ->
            {error, Reason}
    end.

active_alarms(Body) ->
    case row(Body, ?alarms_row) of
        [ {<<"tr">>, _, _} | _T ] ->
            [];
        Alarms when length(Alarms) > 100 ->
            [];
        Alarms ->
            Alarms
    end.

temp(Body) ->
    [_Label, Current, High, HighTime, Low, LowTime] = row(Body, ?temp_row),
    #outside_temp{
        current=to_float(Current),
        high=to_float(High),
        high_time=?STR(HighTime),
        low=to_float(Low),
        low_time=?STR(LowTime)
    }.

%% TOOD: for now this only returns rain last hr
rain(Body) ->
    Row = row(Body, ?rain_row),
    [_Label, _Rate, Day, Storm, _Month, _Year] = Row,
    Row2 = row(Body, ?rain_last_hr_row),
    [_Lablel, Value] = lists:sublist(Row2, 2),
    #rain{ 
        last_hr=to_float(Value),
        last_day=to_float(Day),
        storm=to_float(Storm)
    }.

wind(Body) ->
    [_Label, TwoMin, TenMin | _] = row(Body, ?wind_avg_row),
    [_SpeedLabel, CurrentStr | _] = row(Body, ?wind_speed_row),
    [_DirLabel, Dir | _] = row(Body, ?wind_speed_dir),
    [_StrDir, DegDir] = string:tokens(unicode:characters_to_list(Dir), [160]),
    {Deg, _} = string:to_integer(DegDir),
    #wind{
        current=to_int(CurrentStr),
        direction=Deg,
        two_min_avg=to_float(TwoMin),
        ten_min_avg=to_float(TenMin)
    }.
   
load_sample() ->
    {ok, Body} = file:read_file(?sample1),
    parse_body(Body).

load_sample2() ->
    {ok, Body} = file:read_file(?sample2),
    parse_body(Body).

parse_body(Html) ->
    mochiweb_html:parse(Html).

html_to_record(Html) ->
    B = parse_body(Html),
    #station_summary{
        temp=temp(B),
        rain=rain(B),
        wind=wind(B),
        alarms=active_alarms(B)
    }.

row(Body, I) ->
    mochiweb_xpath:execute(?weather_table_row(I), Body).

%% non exported helpers
to_float(S) ->
    case string:to_float(?STR(S)) of
        {error, _Error} ->
            0.0;
        {F, _Rest} ->
            F
    end.

to_int(S) ->
    case string:to_integer(?STR(S)) of
        {error, _Error} ->
            0;
        {I, _Rest} ->
            I
    end.


%% start unit tests
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

% -define(sample1, lists:flatten(code:priv_dir(?APP) ++ "/summary-sample.html")).
% -define(sample2, lists:flatten(code:priv_dir(?APP) ++ "/summary-sample-no-alarm.html")).

load_test() ->
    {ok, F1} = file:read_file(?sample1),
    % {ok, F2} = file:read_file(?sample2),
    Body1 = parse_body(F1),
    % Body2 = parse_body(F2),

    _Temp1 = temp(Body1),
    _Rain1 = rain(Body1),
    _Wind1 = wind(Body1),

    ok.
    

-endif.
