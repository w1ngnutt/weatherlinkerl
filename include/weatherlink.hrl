%% app name
-define(APP, weatherlink).

-define(STR(A), weatherlink_util:to_str(A)).

%% packet representing a weatherlink archive packet
-record(archive, {
          date,
          time,
          outside_temp,
          high_temp,
          low_temp,
          rainfall,
          high_rain_rate,
          barometer,
          solar_radiation,
          num_wind_samples,
          inside_temp,
          inside_humidity,
          outside_humidity,
          avg_wind_speed,
          high_wind_speed,
          high_wind_speed_dir,
          pervailing_wind_dir,
          avg_uv_index,
          et,
          high_solar_radiation,
          uv_high_index,
          forecast_rule,
          leaf_temp,
          leaf_wetness,
          soil_temps,
          extra_humidities,
          extra_temps,
          soil_moistures
         }).

-record(archive_headers, {
            model,
            num_records,
            max_records,
            archive_interval,
            console_version,
            vantage_tx
         }).

-record(station_summary, {temp, rain, wind, alarms=[]}).
-record(outside_temp, {current, high, high_time, low, low_time}).
-record(rain, {last_hr, last_day, storm}).
-record(wind, {current, direction, two_min_avg, ten_min_avg}).
