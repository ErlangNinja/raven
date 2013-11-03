#!/bin/sh

set -e

START_DIR=`pwd`

cd deps/snowcrash
make clean
cd $START_DIR