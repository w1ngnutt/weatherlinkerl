#!/bin/sh

DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

PZ="-pa ${DIR}/ebin\
    -pa ${DIR}/deps/mochiweb/ebin\
    -pa ${DIR}/deps/mochiweb_xpath/ebin\
    -pa ${DIR}/deps/reloader/ebin"

START="-s weatherlink_app -s reloader"

erl $PZ $START
