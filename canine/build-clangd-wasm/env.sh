#!/usr/bin/env bash
#
# 既にビルドしてあるプロジェクト上で一部だけ更新したい時に使うスクリプト。
# 変数を出力するので eval `env.sh` して使う。
set -e
cd `dirname ${BASH_SOURCE[0]}`
WORKSPACE_DIR=$PWD
OUTPUT_DIR=$WORKSPACE_DIR/../public/static/wasm/
BUILD_DIR=$WORKSPACE_DIR/build
echo WORKSPACE_DIR=$WORKSPACE_DIR
echo OUTPUT_DIR=$OUTPUT_DIR
echo BUILD_DIR=$BUILD_DIR
echo source $BUILD_DIR/emsdk/emsdk_env.sh
