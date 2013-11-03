#!/bin/sh

set -e

START_DIR=`pwd`

rm -f c_src/*.o

cd deps/snowcrash
make clean
cd $START_DIR