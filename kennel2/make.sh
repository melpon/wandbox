#!/bin/bash

set -ex

DIR=`cd $(dirname $0); pwd`

rm -rf build || true
mkdir build

$DIR/cppcms/cppcms/bin/cppcms_tmpl_cc content/root.html.tmpl -o build/root.html.cpp
g++ *.cpp build/*.cpp -o build/kennel -std=gnu++0x -L $DIR/cppcms/cppcms/lib -I $DIR/cppcms/cppcms/include -I . -lcppcms -lbooster -lcrypto -lpcre -lpthread -Wno-deprecated
