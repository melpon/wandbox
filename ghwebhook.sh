#!/bin/bash

su - wandbox -c '
cd wandbox/
git pull

cd cattleshed
make clean
make
cd ../

cd kennel
rm -r dist/
cabal-dev install
cd ../
'
stop kennel
stop cattleshed
sleep 1
start cattleshed
start kennel
