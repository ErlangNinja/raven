#!/bin/sh

set -e

if [ -z "$REBAR_DEPS_DIR" ]
then
        echo "\$REBAR_DEPS_DIR variable is not defined. Run from rebar." 1>&2
        exit 1
fi

START_DIR=`pwd`

rm -f c_src/*.o

if [ -f c_deps/snowcrash/Makefile ]; then
    cd c_deps/snowcrash
    make clean
    cd $START_DIR
fi