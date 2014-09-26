#!/bin/bash

set -ex

cd $(dirname $0)
DIR=`pwd`

rm -rf cppdb || true
mkdir cppdb
cd cppdb

git clone https://github.com/melpon/cppdb source

mkdir build
cd build
cmake ../source/ -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=$DIR/cppdb/cppdb -DDISABLE_MYSQL=ON -DDISABLE_PQ=ON -DDISABLE_ODBC=ON
make
make install
