#!/bin/bash

set -ex

su - wandbox -c '
set -ex

cd wandbox/
git pull
git submodule update -i

cd cattleshed
git clean -xfdq .
autoreconf -i
CC=/usr/local/gcc-4.8.2/bin/gcc CXX="/usr/local/gcc-4.8.2/bin/g++ -static-libstdc++ -static-libgcc" ./configure --prefix=/usr/local/cattleshed --with-boost=/usr/local/boost/boost-1.57.0/gcc-4.8.2
make
echo "exec /usr/local/cattleshed/bin/cattleshed --syslog" > runserver.sh
cd ../

cd kennel2
git clean -xdqf .
./autogen.sh
./configure --prefix=/usr/local/kennel2 --with-cppcms=/usr/local/cppcms --with-cppdb=/usr/local/cppdb
make
cd ../
'
make -C ~wandbox/wandbox/cattleshed install
make -C ~wandbox/wandbox/kennel2 install
stop kennel2 || true
stop cattleshed || true
sleep 1
start cattleshed
start kennel2
