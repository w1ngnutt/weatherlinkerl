%% app name
-define(APP, weatherlink).

-define(STR(A), weatherlink_util:to_str(A)).

%% packet representing a weatherlink LOOP packet
-record(loop_packet, {
        
    }).


-record(station_summary, {temp, rain, wind, alarms=[]}).
-record(outside_temp, {current, high, high_time, low, low_time}).
-record(rain, {last_hr, last_day, storm}).
-record(wind, {current, direction, two_min_avg, ten_min_avg}).
