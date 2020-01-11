#!/bin/bash

set -ex

cd `dirname $0`
INSTALL_DIR="`pwd`/../_install"
MODULE_PATH="`pwd`/../cmake"
PROJECT_DIR="`pwd`"

export PATH=$INSTALL_DIR/cmake/bin:$PATH

mkdir -p _build
pushd _build
  cmake $PROJECT_DIR \
    -DOPENSSL_ROOT_DIR="$INSTALL_DIR/boringssl" \
    -DPCRE_ROOT_DIR="$INSTALL_DIR/pcre" \
    -DCppCMS_ROOT_DIR="$INSTALL_DIR/cppcms" \
    -DCppDB_ROOT_DIR="$INSTALL_DIR/cppdb" \
    -DSQLite3_INCLUDE_DIR="$INSTALL_DIR/sqlite3/include" \
    -DSPDLOG_ROOT_DIR="$INSTALL_DIR/spdlog" \
    -DCMAKE_PREFIX_PATH="$INSTALL_DIR/boost;$INSTALL_DIR/cppcms;$INSTALL_DIR/cppdb;$INSTALL_DIR/zlib;$INSTALL_DIR/curl;$INSTALL_DIR/sqlite3;$INSTALL_DIR/pcre;$INSTALL_DIR/icu;$INSTALL_DIR/cares;$INSTALL_DIR/protobuf;$INSTALL_DIR/grpc" \
    -DCMAKE_MODULE_PATH=$MODULE_PATH \
    -DCMAKE_INSTALL_PREFIX=$PROJECT_DIR/_install \
    -DCMAKE_BUILD_TYPE=Release \
    "$@"
popd
