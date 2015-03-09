#!/bin/sh

DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

PZ="-pz ${DIR}/ebin\
    -pz ${DIR}/deps/ibrowse/ebin\
    -pz ${DIR}/deps/reloader/ebin"
START="-s weatherlinkerl_app -s reloader"

erl $PZ $START
