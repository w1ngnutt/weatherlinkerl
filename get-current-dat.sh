#!/bin/bash

# vantageDateStamp = day + month*32 + (year-2000)*512);
# vantageTimeStamp = (100*hour + minute);

# 416351713 is 06/17/2012 15:05

now=$(date +"%s")
ts=$(($now - 90*60))
echo "NOW: $now"
echo "TS: $ts"
day=$(date -j -f "%s" $ts +"%d")
month=$(date -j -f "%s" $ts +"%m")
year=$(date -j -f "%s" $ts +"%Y")
hour=$(date -j -f "%s" $ts +"%H")
minute=$(date -j -f "%s" $ts +"%M")

# day=17
# month=6
# year=2012
# hour=15
# minute=5

date=$(( $day + ($month<<5) + (($year-2000)<<9) ))
time=$(( $hour*100 + $minute ))
ts=$(( $date << 16 | time ))
echo $ts

#ds = tt[2] + (tt[1]<<5) + ((tt[0]-2000)<<9)

USER=$1
PASS=$2

echo "printing headers"
curl "http://weatherlink.com/webdl.php?timestamp=$ts&user=$USER&pass=$PASS&action=headers"

echo "saving sample"
curl -o priv/sample-loop.dat "http://weatherlink.com/webdl.php?timestamp=$ts&user=$USER&pass=$PASS&action=data"
