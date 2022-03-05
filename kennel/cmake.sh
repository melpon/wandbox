#!/bin/bash

set -ex

cd `dirname $0`
INSTALL_DIR="`pwd`/../_install"
MODULE_PATH="`pwd`/../cmake"
PROJECT_DIR="`pwd`"

BUILD_DIR="_build/local"
GRPC_DIR="$INSTALL_DIR/grpc"
CMAKE_BUILD_TYPE=Debug
ENABLE_TSAN=OFF
ENABLE_ASAN=OFF
CMAKE_INSTALL_PREFIX=$PROJECT_DIR/_install
CMAKE_OPTS=" \
"
LOCAL=1
GDB=0
RUN_AFTER_BUILD=0

while [ $# -ne 0 ]; do
  case "$1" in
    "--help" )
      echo "$0 [--tsan] [--asan] [--local] [--develop] [--master] [--prefix <dir>] [--run] [--gdb] [--help]"
      exit 0
      ;;

    "--prefix" )
      CMAKE_INSTALL_PREFIX="$2"
      shift 1
      ;;

    "--local" )
      LOCAL=1
      BUILD_DIR="_build/local"
      CMAKE_BUILD_TYPE=Debug
      CMAKE_OPTS=" \
      "
      ;;

    "--develop" )
      LOCAL=0
      BUILD_DIR="_build/develop"
      CMAKE_BUILD_TYPE=Release
      CMAKE_OPTS=" \
        -DKENNEL_PORT=3501 \
        -DKENNEL_CATTLESHED_PORT=50052 \
        -DKENNEL_URL=https://develop.wandbox.org \
      "
      ;;
    "--master" )
      LOCAL=0
      BUILD_DIR="_build/master"
      CMAKE_BUILD_TYPE=Release
      CMAKE_OPTS=" \
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

    "--run" )
      RUN_AFTER_BUILD=1
      ;;
    "--gdb" )
      GDB=1
      ;;

    * )
      echo "Unknown option $1" 1>&2
      exit 1
      ;;

  esac
  shift 1
done

export PATH=$INSTALL_DIR/cmake/bin:$PATH

mkdir -p $BUILD_DIR
pushd $BUILD_DIR
  cmake $PROJECT_DIR \
    -DPROTOC_GEN_JSONIF_CPP="$INSTALL_DIR/protoc-gen-jsonif/linux/amd64/protoc-gen-jsonif-cpp" \
    -DCppDB_ROOT_DIR="$INSTALL_DIR/cppdb" \
    -DSQLite3_INCLUDE_DIR="$INSTALL_DIR/sqlite3/include" \
    -DSPDLOG_ROOT_DIR="$INSTALL_DIR/spdlog" \
    -DCLI11_ROOT_DIR="$INSTALL_DIR/CLI11" \
    -DGGRPC_ROOT_DIR="$INSTALL_DIR/ggrpc" \
    -DCMAKE_PREFIX_PATH="$INSTALL_DIR/boost;$INSTALL_DIR/cppdb;$INSTALL_DIR/sqlite3;$GRPC_DIR" \
    -DCMAKE_INSTALL_PREFIX=$CMAKE_INSTALL_PREFIX \
    -DCMAKE_MODULE_PATH=$MODULE_PATH \
    -DCMAKE_BUILD_TYPE=$CMAKE_BUILD_TYPE \
    -DENABLE_TSAN=$ENABLE_TSAN \
    -DENABLE_ASAN=$ENABLE_ASAN \
    $CMAKE_OPTS
  cmake --build . -j`nproc`
popd

if [ $LOCAL -eq 1 -a $RUN_AFTER_BUILD -eq 1 ]; then
  if [ $GDB -eq 1 ]; then
    exec gdb -ex r --args $BUILD_DIR/kennel --log-level debug
  else
    exec $BUILD_DIR/kennel --log-level debug
  fi
fi