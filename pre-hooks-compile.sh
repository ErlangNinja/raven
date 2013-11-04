#!/bin/sh

set -e

START_DIR=`pwd`

cd deps/snowcrash
./configure
make snowcrash
cd $START_DIR