#!/bin/sh

set -e

if [ -z "$REBAR_DEPS_DIR" ]
then
        echo "\$REBAR_DEPS_DIR variable is not defined. Run from rebar." 1>&2
        exit 1
fi

git submodule update --init --recursive
