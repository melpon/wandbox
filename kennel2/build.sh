#!/bin/bash

set -ex

INSTALL_DIR="`pwd`/../_install"
MODULE_PATH="`pwd`/../cmake"
PROJECT_DIR="`pwd`"

mkdir -p build
pushd build
  cmake $PROJECT_DIR \
    -DOPENSSL_ROOT_DIR="$INSTALL_DIR/boringssl" \
    -DPCRE_ROOT_DIR="$INSTALL_DIR/pcre" \
    -DCppCMS_ROOT_DIR="$INSTALL_DIR/cppcms" \
    -DCppDB_ROOT_DIR="$INSTALL_DIR/cppdb" \
    -DSQLite3_INCLUDE_DIR="$INSTALL_DIR/sqlite3/include" \
    -DCMAKE_PREFIX_PATH="$INSTALL_DIR/boost;$INSTALL_DIR/cppcms;$INSTALL_DIR/cppdb;$INSTALL_DIR/zlib;$INSTALL_DIR/curl;$INSTALL_DIR/sqlite3;$INSTALL_DIR/pcre;$INSTALL_DIR/icu" \
    -DCMAKE_MODULE_PATH=$MODULE_PATH
  VERBOSE=1 make
popd
