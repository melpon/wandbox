#!/bin/bash

set -ex

cd `dirname $0`
INSTALL_DIR="`pwd`/../_install"
MODULE_PATH="`pwd`/../cmake"
PROJECT_DIR="`pwd`"

BUILD_DIR="`pwd`/_build/local"
GRPC_DIR="$INSTALL_DIR/grpc"
CMAKE_BUILD_TYPE=Release
ENABLE_TSAN=OFF
ENABLE_ASAN=OFF
CMAKE_INSTALL_PREFIX="$PROJECT_DIR/_install"
CMAKE_OPTS=" \
  -DCATTLESHED_STOREDIR=$BUILD_DIR/cattleshed-develop-log
  -DCATTLESHED_BASEDIR=$BUILD_DIR/cattleshed-develop
  -DCATTLESHED_BINDIR=$BUILD_DIR
"
LOCAL=1
RUN_AFTER_BUILD=0

while [ $# -ne 0 ]; do
  case "$1" in
    "--help" )
      echo "$0 [--tsan] [--asan] [--local] [--develop] [--master] [--prefix <dir>] [--run] [--help]"
      exit 0
      ;;

    "--prefix" )
      CMAKE_INSTALL_PREFIX="$2"
      shift 1
      ;;

    "--local" )
      LOCAL=1
      BUILD_DIR="`pwd`/_build/local"
      CMAKE_OPTS=" \
        -DCATTLESHED_STOREDIR=$BUILD_DIR/cattleshed-develop-log
        -DCATTLESHED_BASEDIR=$BUILD_DIR/cattleshed-develop
        -DCATTLESHED_BINDIR=$BUILD_DIR
      "
      ;;
    "--develop" )
      LOCAL=0
      BUILD_DIR="`pwd`/_build/develop"
      CMAKE_OPTS=" \
        -DCATTLESHED_STOREDIR=/tmp/cattleshed-develop-log
        -DCATTLESHED_BASEDIR=/tmp/cattleshed-develop
        -DCATTLESHED_LISTEN_PORT=50052
      "
      CATTLESHED_BINDIR="$CMAKE_INSTALL_PREFIX/bin"
      ;;
    "--master" )
      LOCAL=0
      BUILD_DIR="`pwd`/_build/master"
      CMAKE_OPTS=" \
      "
      CATTLESHED_BINDIR="$CMAKE_INSTALL_PREFIX/bin"
      ;;

    "--tsan" )
      ENABLE_TSAN=ON
      BUILD_DIR="`pwd`/_build/tsan"
      GRPC_DIR="$INSTALL_DIR/grpc-tsan"
      CMAKE_BUILD_TYPE=Debug
      ;;
    "--asan" )
      ENABLE_ASAN=ON
      BUILD_DIR="`pwd`/_build/asan"
      GRPC_DIR="$INSTALL_DIR/grpc-asan"
      CMAKE_BUILD_TYPE=Debug
      ;;

    "--run" )
      RUN_AFTER_BUILD=1

  esac
  shift 1
done

export PATH=$INSTALL_DIR/cmake/bin:$PATH

mkdir -p $BUILD_DIR
pushd $BUILD_DIR
  cmake $PROJECT_DIR \
    -DCLI11_ROOT_DIR="$INSTALL_DIR/CLI11" \
    -DSPDLOG_ROOT_DIR="$INSTALL_DIR/spdlog" \
    -DGGRPC_ROOT_DIR="$INSTALL_DIR/ggrpc" \
    -DCMAKE_PREFIX_PATH="$INSTALL_DIR/boost;$GRPC_DIR" \
    -DCMAKE_MODULE_PATH=$MODULE_PATH \
    -DCMAKE_INSTALL_PREFIX=$CMAKE_INSTALL_PREFIX \
    -DCMAKE_BUILD_TYPE=$CMAKE_BUILD_TYPE \
    -DENABLE_TSAN=$ENABLE_TSAN \
    -DENABLE_ASAN=$ENABLE_ASAN \
    -DCATTLESHED_BINDIR=$CATTLESHED_BINDIR \
    $CMAKE_OPTS
  cmake --build . -j`nproc`
popd

if [ $LOCAL -eq 1 -a $RUN_AFTER_BUILD -eq 1 ]; then
  sudo setcap cap_sys_admin,cap_chown,cap_setuid,cap_setgid,cap_sys_chroot,cap_mknod,cap_net_admin=p $BUILD_DIR/cattlegrid
  exec $BUILD_DIR/cattleshed -c $BUILD_DIR/cattleshed.conf -c $PROJECT_DIR/compiler.default
fi