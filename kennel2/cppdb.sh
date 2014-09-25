#!/bin/bash

set -ex

DIR=`cd $(dirname $0); pwd`

rm -rf cppdb || true
mkdir cppdb
cd cppdb

git clone https://github.com/melpon/cppdb source

mkdir build
cd build
cmake ../source/ -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=$DIR/cppdb/cppdb -DDISABLE_MYSQL=ON -DSQLITE_BACKEND_INTERNAL=ON -DDISABLE_PQ=ON -DDISABLE_ODBC=ON
make
make install
