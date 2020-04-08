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

while [ $# -ne 0 ]; do
  case "$1" in
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
    -DCMAKE_PREFIX_PATH="$INSTALL_DIR/boost;$INSTALL_DIR/cppcms;$INSTALL_DIR/cppdb;$INSTALL_DIR/curl;$INSTALL_DIR/sqlite3;$INSTALL_DIR/pcre;$INSTALL_DIR/icu;$GRPC_DIR" \
    -DCMAKE_MODULE_PATH=$MODULE_PATH \
    -DCMAKE_INSTALL_PREFIX=$PROJECT_DIR/_install \
    -DCMAKE_BUILD_TYPE=$CMAKE_BUILD_TYPE \
    -DENABLE_TSAN=$ENABLE_TSAN \
    -DENABLE_ASAN=$ENABLE_ASAN
  cmake --build . -j`nproc`
  sudo setcap cap_sys_admin,cap_sys_chroot,cap_mknod,cap_net_admin=p cattleshed/cattlegrid

  rm -rf /var/crash/*cattleshed.*.crash
  rm -rf /var/crash/*kennel.*.crash

  ctest . -V
popd
