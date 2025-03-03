#!/usr/bin/env bash
# 以下のファイルをこのプロジェクト用に改造したスクリプトです。
#
# https://github.com/guyutongxue/clangd-in-browser/blob/f4ad53f8d6cb8781417c26106290eaa8a619876b/build.sh
#
# 元のライセンスは以下の通り。
#
# Copyright (c) 2024 Guyutongxue
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.


# It's not recommend for you to run this script directly,
# (because I'm not good at writing this sorry)
# but you can use it as a reference for building.

set -ex

# 0. Configs

# sudo apt install vim git build-essential cmake ninja-build python3

## Note: Better to make sure WASI SDK version matches the LLVM version
EMSDK_VER=3.1.52
WASI_SDK_VER=22.0
WASI_SDK_VER_MAJOR=22
LLVM_VER=18.1.2
LLVM_VER_MAJOR=18

cd `dirname $0`
WORKSPACE_DIR=$PWD
OUTPUT_DIR=$WORKSPACE_DIR/../public/static/wasm/
OUTPUT_R2_DIR=$WORKSPACE_DIR/../public-r2/
BULID_DIR=$WORKSPACE_DIR/../../build-clangd-wasm-build
rm -rf $BULID_DIR
mkdir -p $BULID_DIR
cd $BULID_DIR

# 1. Get Emscripten

git clone --branch $EMSDK_VER --depth 1 https://github.com/emscripten-core/emsdk
pushd emsdk
  ./emsdk install $EMSDK_VER
  ./emsdk activate $EMSDK_VER
  source ./emsdk_env.sh
popd

# 2. Prepare WASI sysroot

wget -O- https://github.com/WebAssembly/wasi-sdk/releases/download/wasi-sdk-$WASI_SDK_VER_MAJOR/wasi-sysroot-$WASI_SDK_VER.tar.gz | tar -xz

# 3. Build LLVM

git clone --branch llvmorg-$LLVM_VER --depth 1 https://github.com/llvm/llvm-project
pushd llvm-project
  ## Build native tools first
  cmake -G Ninja -S llvm -B build-native \
      -DCMAKE_BUILD_TYPE=Release \
      -DLLVM_ENABLE_PROJECTS=clang
  cmake --build build-native --target llvm-tblgen clang-tblgen

  ## Apply a patch for blocking stdin read
  git apply $WORKSPACE_DIR/wait_stdin.patch

  ## Build clangd (1st time, just for compiler headers)
  emcmake cmake -G Ninja -S llvm -B build \
      -DCMAKE_CXX_FLAGS="-pthread -Dwait4=__syscall_wait4" \
      -DCMAKE_EXE_LINKER_FLAGS="-pthread -s ENVIRONMENT=worker -s NO_INVOKE_RUN" \
      -DCMAKE_BUILD_TYPE=MinSizeRel \
      -DLLVM_TARGET_ARCH=wasm32-emscripten \
      -DLLVM_DEFAULT_TARGET_TRIPLE=wasm32-wasi \
      -DLLVM_TARGETS_TO_BUILD=WebAssembly \
      -DLLVM_ENABLE_PROJECTS="clang;clang-tools-extra" \
      -DLLVM_TABLEGEN=$PWD/build-native/bin/llvm-tblgen \
      -DCLANG_TABLEGEN=$PWD/build-native/bin/clang-tblgen \
      -DLLVM_BUILD_STATIC=ON \
      -DLLVM_INCLUDE_EXAMPLES=OFF \
      -DLLVM_INCLUDE_TESTS=OFF \
      -DLLVM_ENABLE_BACKTRACES=OFF \
      -DLLVM_ENABLE_UNWIND_TABLES=OFF \
      -DLLVM_ENABLE_CRASH_OVERRIDES=OFF \
      -DCLANG_ENABLE_STATIC_ANALYZER=OFF \
      -DLLVM_ENABLE_TERMINFO=OFF \
      -DLLVM_ENABLE_PIC=OFF \
      -DLLVM_ENABLE_ZLIB=OFF \
      -DCLANG_ENABLE_ARCMT=OFF
  cmake --build build --target clangd

  ## Copy installed headers to WASI sysroot
  cp -r build/lib/clang/$LLVM_VER_MAJOR/include/* $BULID_DIR/wasi-sysroot/include/

  mkdir -p $BULID_DIR/wasi-sysroot/wandbox/

  ## Boost のヘッダーを入れる
  rm -rf boost_1_87_0.tar.gz boost_1_87_0
  curl -LO https://archives.boost.io/release/1.87.0/source/boost_1_87_0.tar.gz
  tar xf boost_1_87_0.tar.gz
  cp -r boost_1_87_0/boost $BULID_DIR/wasi-sysroot/wandbox/boost/

  ## Build clangd (2nd time, for the real thing)
  emcmake cmake -G Ninja -S llvm -B build \
      -DCMAKE_CXX_FLAGS="-pthread -Dwait4=__syscall_wait4" \
      -DCMAKE_EXE_LINKER_FLAGS="-pthread -s ENVIRONMENT=worker -s NO_INVOKE_RUN -s EXIT_RUNTIME -s INITIAL_MEMORY=2GB -s ALLOW_MEMORY_GROWTH -s MAXIMUM_MEMORY=4GB -s STACK_SIZE=256kB -s EXPORTED_RUNTIME_METHODS=FS,callMain -s MODULARIZE -s EXPORT_ES6 -s WASM_BIGINT -s ASSERTIONS -s ASYNCIFY -s PTHREAD_POOL_SIZE='Math.max(navigator.hardwareConcurrency, 8)' --embed-file=$BULID_DIR/wasi-sysroot/include@/usr/include --embed-file=$BULID_DIR/wasi-sysroot/wandbox@/usr/local/include/wandbox" \
      -DCMAKE_BUILD_TYPE=MinSizeRel \
      -DLLVM_TARGET_ARCH=wasm32-emscripten \
      -DLLVM_DEFAULT_TARGET_TRIPLE=wasm32-wasi-threads \
      -DLLVM_TARGETS_TO_BUILD=WebAssembly \
      -DLLVM_ENABLE_PROJECTS="clang;clang-tools-extra" \
      -DLLVM_TABLEGEN=$PWD/build-native/bin/llvm-tblgen \
      -DCLANG_TABLEGEN=$PWD/build-native/bin/clang-tblgen \
      -DLLVM_BUILD_STATIC=ON \
      -DLLVM_INCLUDE_EXAMPLES=OFF \
      -DLLVM_INCLUDE_TESTS=OFF \
      -DLLVM_ENABLE_BACKTRACES=OFF \
      -DLLVM_ENABLE_UNWIND_TABLES=OFF \
      -DLLVM_ENABLE_CRASH_OVERRIDES=OFF \
      -DCLANG_ENABLE_STATIC_ANALYZER=OFF \
      -DLLVM_ENABLE_TERMINFO=OFF \
      -DLLVM_ENABLE_PIC=OFF \
      -DLLVM_ENABLE_ZLIB=OFF \
      -DCLANG_ENABLE_ARCMT=OFF
  cmake --build build --target clangd
popd

# 4. Copy the final binary
rm -rf $OUTPUT_DIR
mkdir -p $OUTPUT_DIR
cp llvm-project/build/bin/clangd* $OUTPUT_DIR
# clangd.wasm は R2 用のディレクトリに配置する
rm $OUTPUT_DIR/clangd.wasm
cp llvm-project/build/bin/clangd.wasm $OUTPUT_R2_DIR
