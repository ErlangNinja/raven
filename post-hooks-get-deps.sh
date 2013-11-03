#!/bin/sh

set -e

START_DIR=`pwd`

cd deps/snowcrash
git submodule update --init --recursive
cd $START_DIR