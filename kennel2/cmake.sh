#!/bin/bash

set -ex

cd `dirname $0`
INSTALL_DIR="`pwd`/../_install"
MODULE_PATH="`pwd`/../cmake"
PROJECT_DIR="`pwd`"

BUILD_DIR="_build/release"
GRPC_DIR="$INSTALL_DIR/grpc"
CMAKE_BUILD_TYPE=Release
ENABLE_TSAN=OFF
ENABLE_ASAN=OFF
CMAKE_INSTALL_PREFIX=$PROJECT_DIR/_install
CMAKE_OPTS=""

while [ $# -ne 0 ]; do
  case "$1" in
    "--help" )
      echo "$0 [--tsan] [--asan] [--staging] [--production] [--prefix <dir>] [--help]"
      exit 0
      ;;

    "--prefix" )
      CMAKE_INSTALL_PREFIX="$2"
      shift 1
      ;;

    "--staging" )
      CMAKE_OPTS=" \
        -DKENNEL_GOOGLEANALYTICS=UA-56896607-3 \
        -DKENNEL_GITHUBCLIENT=f9d429d939d997e6b08e \
        -DKENNEL_DOMAIN=staging.wandbox.org \
        -DKENNEL_CATTLESHED_PORT=50052 \
        -DKENNEL_SERVICE_PORT=3501 \
      "
      ;;
    "--production" )
      CMAKE_OPTS=" \
        -DKENNEL_GOOGLEANALYTICS=UA-56896607-3 \
        -DKENNEL_GITHUBCLIENT=f9d429d939d997e6b08e \
      "
      ;;

    "--tsan" )
      ENABLE_TSAN=ON
      BUILD_DIR="_build/tsan"
      GRPC_DIR="$INSTALL_DIR/grpc-tsan"
      CMAKE_BUILD_TYPE=Debug
      ;;
    "--asan" )
      ENABLE_ASAN=ON
      BUILD_DIR="_build/asan"
      GRPC_DIR="$INSTALL_DIR/grpc-asan"
      CMAKE_BUILD_TYPE=Debug
      ;;
  esac
  shift 1
done

export PATH=$INSTALL_DIR/cmake/bin:$PATH

mkdir -p $BUILD_DIR
pushd $BUILD_DIR
  cmake $PROJECT_DIR \
    -DPCRE_ROOT_DIR="$INSTALL_DIR/pcre" \
    -DCppCMS_ROOT_DIR="$INSTALL_DIR/cppcms" \
    -DCppDB_ROOT_DIR="$INSTALL_DIR/cppdb" \
    -DSQLite3_INCLUDE_DIR="$INSTALL_DIR/sqlite3/include" \
    -DSPDLOG_ROOT_DIR="$INSTALL_DIR/spdlog" \
    -DCLI11_ROOT_DIR="$INSTALL_DIR/CLI11" \
    -DCMAKE_PREFIX_PATH="$INSTALL_DIR/boost;$INSTALL_DIR/cppcms;$INSTALL_DIR/cppdb;$INSTALL_DIR/curl;$INSTALL_DIR/sqlite3;$INSTALL_DIR/pcre;$INSTALL_DIR/icu;$GRPC_DIR" \
    -DCMAKE_INSTALL_PREFIX=$CMAKE_INSTALL_PREFIX \
    -DCMAKE_MODULE_PATH=$MODULE_PATH \
    -DCMAKE_BUILD_TYPE=$CMAKE_BUILD_TYPE \
    -DENABLE_TSAN=$ENABLE_TSAN \
    -DENABLE_ASAN=$ENABLE_ASAN \
    $CMAKE_OPTS
  cmake --build . -j`nproc`
popd
