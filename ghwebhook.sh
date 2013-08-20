#!/bin/bash

su - wandbox -c '
set -ex

cd wandbox/
git pull

cd cattleshed
git clean -xfdq .
autoreconf -i
./configure --prefix=/usr/local/cattleshed --with-boost=/usr/local/boost-1.54.0
make
echo "exec /usr/local/cattleshed/bin/cattleshed" > runserver.sh
cd ../

cd kennel
rm -r dist/
cabal-dev install
cd ../
' && \
make -C wandbox/cattleshed install
stop kennel
stop cattleshed
sleep 1
start cattleshed
start kennel
