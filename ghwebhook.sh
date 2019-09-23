#!/bin/bash

set -ex

su - wandbox -c '
set -ex

pushd wandbox
  git pull
  git submodule update -i

  ./install_tools.sh

  pushd kennel2
    ./cmake.sh -DKENNEL_GOOGLEANALYTICS=UA-56896607-3 -DKENNEL_GITHUBCLIENT=f9d429d939d997e6b08e
    make -C _build
  popd
popd
'
make -C ~wandbox/wandbox/kennel2 install
systemctl stop kennel
sleep 1
systemctl start kennel
