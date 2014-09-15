#!/bin/bash

set -ex

DIR=`cd $(dirname $0); pwd`

rm -rf cppcms || true
mkdir cppcms
cd cppcms

git clone https://github.com/melpon/cppcms source
cd source
tar xf cppcms_boost.tar.bz2
cd ..

mkdir build
cd build
cmake ../source/ -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=$DIR/cppcms/cppcms -DDISABLE_SHARED=ON -DDISABLE_FCGI=ON -DDISABLE_SCGI=ON -DDISABLE_ICU_LOCALE=ON -DDISABLE_TCPCACHE=ON
make
make install
