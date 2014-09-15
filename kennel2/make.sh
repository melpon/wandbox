#!/bin/bash

set -ex

DIR=`cd $(dirname $0); pwd`

rm -rf build || true
mkdir build

$DIR/cppcms/cppcms/bin/cppcms_tmpl_cc my_skin.tmpl -o build/my_skin.cpp
g++ *.cpp build/*.cpp -o build/kennel -L $DIR/cppcms/cppcms/lib -I $DIR/cppcms/cppcms/include -I . -lcppcms -lbooster -lcrypto -lpcre -lpthread
