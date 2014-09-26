#!/bin/bash

set -ex

DIR=`cd $(dirname $0); pwd`

rm -rf build || true
mkdir build

$DIR/cppcms/cppcms/bin/cppcms_tmpl_cc src/root.html.tmpl -o build/root.html.cpp
g++ src/*.cpp build/*.cpp -o build/kennel -std=gnu++0x -L $DIR/cppcms/cppcms/lib -L $DIR/cppdb/cppdb/lib -I $DIR/cppcms/cppcms/include -I $DIR/cppdb/cppdb/include -I src -lcppcms -lbooster -lcrypto -lpcre -lpthread -lcppdb -lsqlite3 -lz -ldl -Wno-deprecated -static-libgcc -static-libstdc++ -static
