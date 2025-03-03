#!/usr/bin/env bash
#
# 既にビルドしてあるプロジェクト上で一部だけ更新したい時に使うスクリプト。
# 変数を出力するので eval `env.sh` して使う。
set -e
cd `dirname ${BASH_SOURCE[0]}`
WORKSPACE_DIR=$PWD
OUTPUT_DIR=$WORKSPACE_DIR/../public/static/wasm/
OUTPUT_R2_DIR=$WORKSPACE_DIR/../public-r2/
BUILD_DIR=$WORKSPACE_DIR/../../build-clangd-wasm-build
echo WORKSPACE_DIR=$WORKSPACE_DIR
echo OUTPUT_DIR=$OUTPUT_DIR
echo OUTPUT_R2_DIR=$OUTPUT_R2_DIR
echo BUILD_DIR=$BUILD_DIR
echo source $BUILD_DIR/emsdk/emsdk_env.sh
