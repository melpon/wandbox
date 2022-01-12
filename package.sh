#!/bin/bash

PROG=$0

function show_help() {
  echo "$PROG <kennel | cattleshed> <develop | master>"
}

if [ $# -lt 2 ]; then
  show_help
  exit 1
fi

APP=$1
ENV=$2

cd `dirname $0`
INSTALL_DIR="`pwd`/_install"

set -ex

if [ "$APP" != "kennel" -a "$APP" != "cattleshed" -a "$APP" != "canine" ]; then
  show_help
  exit 1
fi

if [ "$ENV" != "develop" -a "$ENV" != "master" ]; then
  show_help
  exit 1
fi

PREFIX="/opt/wandbox-data/release/$APP-$ENV"

./install_deps.sh

if [ "$APP" = "kennel" ]; then
  pushd kennel2
    rm -rf _build/release
    ./cmake.sh --prefix $PREFIX --$ENV
    sudo $INSTALL_DIR/cmake/bin/cmake --install _build/release
  popd
elif [ "$APP" = "cattleshed" ]; then
  pushd cattleshed
    rm -rf _build/release
    ./cmake.sh --prefix $PREFIX --$ENV
    sudo $INSTALL_DIR/cmake/bin/cmake --install _build/release
  popd
elif [ "$APP" = "canine" ]; then
  pushd canine
    npm ci
    npm run dist
    mkdir -p $PREFIX
    cp -r dist/* $PREFIX/
  popd
fi

mkdir -p _package
TARFILE="`pwd`/_package/$APP-$ENV.tar.gz"
pushd `dirname $PREFIX`
  tar czf $TARFILE `basename $PREFIX`
popd
sudo rm -rf $PREFIX
