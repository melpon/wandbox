#!/bin/bash

set -ex

cd $(dirname $0)
DIR=`pwd`

PREFIX=$DIR/cppcms/cppcms
# PREFIX=/usr/local/cppcms

rm -rf cppcms || true
mkdir cppcms
cd cppcms

git clone https://github.com/melpon/cppcms source

mkdir build
cd build
cmake ../source/ -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=$PREFIX -DDISABLE_SHARED=ON -DDISABLE_SCGI=ON -DDISABLE_ICU_LOCALE=ON -DDISABLE_TCPCACHE=ON
make
make install
