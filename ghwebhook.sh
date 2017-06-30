#!/bin/bash

set -ex

su - wandbox -c '
set -ex

cd wandbox/
git pull
git submodule update -i

cd kennel2
git clean -xdqf .
./autogen.sh
./configure --prefix=/usr/local/kennel2 --with-cppcms=/usr/local/cppcms --with-cppdb=/usr/local/cppdb --with-sponsors=/usr/local/kennel2/etc/sponsors.json --with-googleanalytics=UA-56896607-3 --with-githubclient=f9d429d939d997e6b08e
make
cd ../
'
make -C ~wandbox/wandbox/kennel2 install
stop kennel2 || true
sleep 1
start kennel2
