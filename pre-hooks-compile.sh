#!/bin/sh

set -e

if [ -z "$REBAR_DEPS_DIR" ]
then
        echo "\$REBAR_DEPS_DIR variable is not defined. Run from rebar." 1>&2
        exit 1
fi

START_DIR=`pwd`

if [ -d c_deps/snowcrash ]; then
    cd c_deps/snowcrash
    ./configure
    make snowcrash
    cd $START_DIR
fi