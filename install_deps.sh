#!/bin/bash

SOURCE_DIR="`pwd`/_source"
BUILD_DIR="`pwd`/_build"
INSTALL_DIR="`pwd`/_install"

set -ex

mkdir -p $SOURCE_DIR
mkdir -p $BUILD_DIR
mkdir -p $INSTALL_DIR

RELEASE_MODE=0
if [ "$1" = "--release" ]; then
  RELEASE_MODE=1
fi

CMAKE_VERSION="3.17.0"
CMAKE_VERSION_FILE="$INSTALL_DIR/cmake.version"
CMAKE_CHANGED=0
if [ ! -e $CMAKE_VERSION_FILE -o "$CMAKE_VERSION" != "`cat $CMAKE_VERSION_FILE`" ]; then
  CMAKE_CHANGED=1
fi

BOOST_VERSION="1.72.0"
BOOST_VERSION_FILE="$INSTALL_DIR/boost.version"
BOOST_CHANGED=0
if [ ! -e $BOOST_VERSION_FILE -o "$BOOST_VERSION" != "`cat $BOOST_VERSION_FILE`" ]; then
  BOOST_CHANGED=1
fi

ICU_VERSION="64.2"
ICU_VERSION_FILE="$INSTALL_DIR/icu.version"
ICU_CHANGED=0
if [ ! -e $ICU_VERSION_FILE -o "$ICU_VERSION" != "`cat $ICU_VERSION_FILE`" ]; then
  ICU_CHANGED=1
fi

CPPCMS_VERSION="1.2.1"
CPPCMS_VERSION_FILE="$INSTALL_DIR/cppcms.version"
CPPCMS_CHANGED=0
if [ ! -e $CPPCMS_VERSION_FILE -o "$CPPCMS_VERSION" != "`cat $CPPCMS_VERSION_FILE`" ]; then
  CPPCMS_CHANGED=1
fi

CPPDB_VERSION="0.3.1"
CPPDB_VERSION_FILE="$INSTALL_DIR/cppdb.version"
CPPDB_CHANGED=0
if [ ! -e $CPPDB_VERSION_FILE -o "$CPPDB_VERSION" != "`cat $CPPDB_VERSION_FILE`" ]; then
  CPPDB_CHANGED=1
fi

GRPC_VERSION="1.28.0"
GRPC_VERSION_FILE="$INSTALL_DIR/grpc.version"
GRPC_CHANGED=0
if [ ! -e $GRPC_VERSION_FILE -o "$GRPC_VERSION" != "`cat $GRPC_VERSION_FILE`" ]; then
  GRPC_CHANGED=1
fi

PCRE_VERSION="8.43"
PCRE_VERSION_FILE="$INSTALL_DIR/pcre.version"
PCRE_CHANGED=0
if [ ! -e $PCRE_VERSION_FILE -o "$PCRE_VERSION" != "`cat $PCRE_VERSION_FILE`" ]; then
  PCRE_CHANGED=1
fi

CURL_VERSION="7.66.0"
CURL_VERSION_FILE="$INSTALL_DIR/curl.version"
CURL_CHANGED=0
if [ ! -e $CURL_VERSION_FILE -o "$CURL_VERSION" != "`cat $CURL_VERSION_FILE`" ]; then
  CURL_CHANGED=1
fi

SQLITE3_VERSION="3.29.0"
SQLITE3_VERSION_NUMBER="3290000"
SQLITE3_VERSION_FILE="$INSTALL_DIR/sqlite3.version"
SQLITE3_CHANGED=0
if [ ! -e $SQLITE3_VERSION_FILE -o "$SQLITE3_VERSION" != "`cat $SQLITE3_VERSION_FILE`" ]; then
  SQLITE3_CHANGED=1
fi

CLI11_VERSION="1.8.0"
CLI11_VERSION_FILE="$INSTALL_DIR/cli11.version"
CLI11_CHANGED=0
if [ ! -e $CLI11_VERSION_FILE -o "$CLI11_VERSION" != "`cat $CLI11_VERSION_FILE`" ]; then
  CLI11_CHANGED=1
fi

SPDLOG_VERSION="1.4.2"
SPDLOG_VERSION_FILE="$INSTALL_DIR/spdlog.version"
SPDLOG_CHANGED=0
if [ ! -e $SPDLOG_VERSION_FILE -o "$SPDLOG_VERSION" != "`cat $SPDLOG_VERSION_FILE`" ]; then
  SPDLOG_CHANGED=1
fi

GGRPC_VERSION="0.1.0"
GGRPC_VERSION_FILE="$INSTALL_DIR/ggrpc.version"
GGRPC_CHANGED=0
if [ ! -e $GGRPC_VERSION_FILE -o "$GGRPC_VERSION" != "`cat $GGRPC_VERSION_FILE`" ]; then
  GGRPC_CHANGED=1
fi

if [ -z "$JOBS" ]; then
  # Linux
  JOBS=`nproc 2>/dev/null`
  if [ -z "$JOBS" ]; then
    # macOS
    JOBS=`sysctl -n hw.logicalcpu_max 2>/dev/null`
    if [ -z "$JOBS" ]; then
      JOBS=1
    fi
  fi
fi

# CMake が古いとビルド出来ないので、インストール済み CMake から新しい CMake をインストールする
if [ $CMAKE_CHANGED -eq 1 -o ! -e $INSTALL_DIR/cmake/bin/cmake ]; then
  _URL=https://github.com/Kitware/CMake/releases/download/v${CMAKE_VERSION}/cmake-${CMAKE_VERSION}-Linux-x86_64.tar.gz
  _FILE=$SOURCE_DIR/cmake-${CMAKE_VERSION}-Linux-x86_64.tar.gz
  if [ ! -e $_FILE ]; then
    echo "file(DOWNLOAD $_URL $_FILE)" > $SOURCE_DIR/tmp.cmake
    cmake -P $SOURCE_DIR/tmp.cmake
    rm $SOURCE_DIR/tmp.cmake
  fi

  pushd $SOURCE_DIR
    rm -rf cmake-${CMAKE_VERSION}-Linux-x86_64
    cmake -E tar xf $_FILE
  popd

  rm -rf $INSTALL_DIR/cmake
  mv $SOURCE_DIR/cmake-${CMAKE_VERSION}-Linux-x86_64 $INSTALL_DIR/cmake
fi
echo $CMAKE_VERSION > $CMAKE_VERSION_FILE

export PATH=$INSTALL_DIR/cmake/bin:$PATH

# grpc (cmake)
if [ $GRPC_CHANGED -eq 1 -o ! -e $INSTALL_DIR/grpc/lib/libgrpc++_unsecure.a ]; then
  # gRPC のソース取得
  if [ ! -e $SOURCE_DIR/grpc/.git ]; then
    git clone https://github.com/grpc/grpc.git $SOURCE_DIR/grpc
  fi
  pushd $SOURCE_DIR/grpc
    git fetch
    git reset --hard v$GRPC_VERSION
    git submodule update -i --recursive
  popd

  # RELEASE_MODE=1 の場合は tsan, asan は入れない
  _BUILDTYPE="release tsan asan"
  if [ $RELEASE_MODE -eq 1 ]; then
    _BUILDTYPE="release"
  fi
  for buildtype in $_BUILDTYPE; do
    case "$buildtype" in
      "release" )
        _POSTFIX=""
        _OPTS="
          -DCMAKE_BUILD_TYPE=Release \
        "
        ;;
      "tsan" )
        _POSTFIX="-tsan"
        _OPTS="
          -DCMAKE_BUILD_TYPE=Debug \
          -DCMAKE_C_FLAGS="-fsanitize=thread" \
          -DCMAKE_CXX_FLAGS="-fsanitize=thread" \
        "
        ;;
      "asan" )
        _POSTFIX="-asan"
        _OPTS="
          -DCMAKE_BUILD_TYPE=Debug \
          -DCMAKE_C_FLAGS="-fsanitize=address" \
          -DCMAKE_CXX_FLAGS="-fsanitize=address" \
        "
        ;;
    esac

    rm -rf $BUILD_DIR/grpc-build$_POSTFIX
    mkdir -p $BUILD_DIR/grpc-build$_POSTFIX
    pushd $BUILD_DIR/grpc-build$_POSTFIX
      cmake $SOURCE_DIR/grpc \
        -DCMAKE_INSTALL_PREFIX=$INSTALL_DIR/grpc$_POSTFIX \
        -DgRPC_BUILD_CSHARP_EXT=OFF \
        $_OPTS
      make -j$JOBS
      make install
      # boringssl のヘッダーも入れておく
      cp -r $SOURCE_DIR/grpc/third_party/boringssl-with-bazel/src/include/openssl $INSTALL_DIR/grpc/include/openssl
    popd
  done
fi
echo $GRPC_VERSION > $GRPC_VERSION_FILE

# boost
if [ $BOOST_CHANGED -eq 1 -o ! -e $INSTALL_DIR/boost/lib/libboost_filesystem.a ]; then
  _VERSION_UNDERSCORE=${BOOST_VERSION//./_}
  _URL=https://dl.bintray.com/boostorg/release/${BOOST_VERSION}/source/boost_${_VERSION_UNDERSCORE}.tar.gz
  _FILE=$SOURCE_DIR/boost_${_VERSION_UNDERSCORE}.tar.gz
  if [ ! -e $_FILE ]; then
    echo "file(DOWNLOAD $_URL $_FILE)" > $BUILD_DIR/tmp.cmake
    cmake -P $BUILD_DIR/tmp.cmake
    rm $BUILD_DIR/tmp.cmake
  fi
  pushd $SOURCE_DIR
    rm -rf boost_${_VERSION_UNDERSCORE}
    cmake -E tar xf $_FILE
  popd

  pushd $SOURCE_DIR/boost_${_VERSION_UNDERSCORE}
    ./bootstrap.sh
    ./b2 install \
      --prefix=$INSTALL_DIR/boost \
      --build-dir=$BUILD_DIR/boost-build \
      --with-filesystem \
      --with-program_options \
      link=static
  popd
fi
echo $BOOST_VERSION > $BOOST_VERSION_FILE

# icu
if [ $ICU_CHANGED -eq 1 -o ! -e $INSTALL_DIR/icu/lib/libicudata.a ]; then
  _VERSION_UNDERSCORE=${ICU_VERSION//./_}
  _VERSION_MINUS=${ICU_VERSION//./-}
  _URL=https://github.com/unicode-org/icu/releases/download/release-$_VERSION_MINUS/icu4c-${_VERSION_UNDERSCORE}-src.tgz
  _FILE=$SOURCE_DIR/icu4c-${_VERSION_UNDERSCORE}-src.tgz
  if [ ! -e $_FILE ]; then
    echo "file(DOWNLOAD $_URL $_FILE)" > $BUILD_DIR/tmp.cmake
    cmake -P $BUILD_DIR/tmp.cmake
    rm $BUILD_DIR/tmp.cmake
  fi

  pushd $SOURCE_DIR
    rm -rf icu
    cmake -E tar xf $_FILE
  popd

  rm -rf $BUILD_DIR/icu-build
  mkdir $BUILD_DIR/icu-build
  pushd $BUILD_DIR/icu-build
    $SOURCE_DIR/icu/source/configure \
      --prefix=$INSTALL_DIR/icu \
      --disable-tests \
      --disable-samples \
      --disable-shared \
      --enable-static
    make -j$JOBS
    make install
  popd
fi
echo $ICU_VERSION > $ICU_VERSION_FILE

# pcre
if [ $PCRE_CHANGED -eq 1 -o ! -e $INSTALL_DIR/pcre/lib/libpcre.a ]; then
  _URL=https://ftp.pcre.org/pub/pcre/pcre-$PCRE_VERSION.zip
  _FILE=$SOURCE_DIR/pcre-$PCRE_VERSION.zip
  if [ ! -e $_FILE ]; then
    echo "file(DOWNLOAD $_URL $_FILE)" > $BUILD_DIR/tmp.cmake
    cmake -P $BUILD_DIR/tmp.cmake
    rm $BUILD_DIR/tmp.cmake
  fi

  pushd $SOURCE_DIR
    rm -rf pcre-$PCRE_VERSION
    cmake -E tar xf $_FILE
  popd

  rm -rf $BUILD_DIR/pcre-build
  mkdir -p $BUILD_DIR/pcre-build
  pushd $BUILD_DIR/pcre-build
    cmake $SOURCE_DIR/pcre-$PCRE_VERSION \
      -DCMAKE_BUILD_TYPE=Release \
      -DCMAKE_INSTALL_PREFIX=$INSTALL_DIR/pcre \
      -DPCRE_SUPPORT_UTF=ON \
      -DPCRE_BUILD_PCREGREP=OFF \
      -DPCRE_BUILD_TESTS=OFF
    make -j$JOBS
    make install
  popd
fi
echo $PCRE_VERSION > $PCRE_VERSION_FILE

# curl
if [ $CURL_CHANGED -eq 1 -o ! -e $INSTALL_DIR/curl/lib/libcurl.a ]; then
  _VERSION_UNDERSCORE=${CURL_VERSION//./_}
  _URL=https://github.com/curl/curl/releases/download/curl-${_VERSION_UNDERSCORE}/curl-$CURL_VERSION.tar.gz
  _FILE=$SOURCE_DIR/curl-$CURL_VERSION.tar.gz
  if [ ! -e $_FILE ]; then
    echo "file(DOWNLOAD $_URL $_FILE)" > $BUILD_DIR/tmp.cmake
    cmake -P $BUILD_DIR/tmp.cmake
    rm $BUILD_DIR/tmp.cmake
  fi

  pushd $SOURCE_DIR
    rm -rf curl-$CURL_VERSION
    cmake -E tar xf $_FILE
  popd

  rm -rf $BUILD_DIR/curl-build
  mkdir -p $BUILD_DIR/curl-build
  pushd $BUILD_DIR/curl-build
    $SOURCE_DIR/curl-$CURL_VERSION/configure \
      --prefix=$INSTALL_DIR/curl \
      --disable-shared \
      --disable-ldap \
      --with-ssl=$INSTALL_DIR/grpc \
      --with-zlib=$INSTALL_DIR/grpc \
      --without-librtmp
    make -j$JOBS
    make install
  popd
fi
echo $CURL_VERSION > $CURL_VERSION_FILE

# sqlite3
if [ $SQLITE3_CHANGED -eq 1 -o ! -e $INSTALL_DIR/sqlite3/lib/libsqlite3.a ]; then
  _URL=https://www.sqlite.org/2019/sqlite-autoconf-$SQLITE3_VERSION_NUMBER.tar.gz
  _FILE=$SOURCE_DIR/sqlite-autoconf-$SQLITE3_VERSION_NUMBER.tar.gz
  if [ ! -e $_FILE ]; then
    echo "file(DOWNLOAD $_URL $_FILE)" > $BUILD_DIR/tmp.cmake
    cmake -P $BUILD_DIR/tmp.cmake
    rm $BUILD_DIR/tmp.cmake
  fi

  pushd $SOURCE_DIR
    rm -rf sqlite-autoconf-$SQLITE3_VERSION_NUMBER
    cmake -E tar xf $_FILE
  popd

  rm -rf $BUILD_DIR/sqlite-build
  mkdir -p $BUILD_DIR/sqlite-build
  pushd $BUILD_DIR/sqlite-build
    $SOURCE_DIR/sqlite-autoconf-$SQLITE3_VERSION_NUMBER/configure \
      --prefix=$INSTALL_DIR/sqlite3 \
      --disable-shared \
      --enable-static
    make -j$JOBS
    make install
  popd
fi
echo $SQLITE3_VERSION > $SQLITE3_VERSION_FILE

# cppcms
if [ $CPPCMS_CHANGED -eq 1 -o ! -e $INSTALL_DIR/cppcms/lib/libcppcms.a ]; then
  rm -rf $SOURCE_DIR/cppcms
  git clone --branch v$CPPCMS_VERSION --depth 1 https://github.com/artyom-beilis/cppcms.git $SOURCE_DIR/cppcms

  PATCH_DIR=`pwd`/patch
  # パッチの適用
  pushd $SOURCE_DIR/cppcms
    patch -p1 < $PATCH_DIR/001_http_protocol.patch
    patch -p1 < $PATCH_DIR/002_ignore_http_header_comments.patch
    patch -p1 < $PATCH_DIR/003_cxx11_notest.patch
  popd

  # ビルドとインストール
  mkdir -p $BUILD_DIR/cppcms-build
  pushd $BUILD_DIR/cppcms-build
    cmake $SOURCE_DIR/cppcms \
      -DCMAKE_BUILD_TYPE=Release \
      -DCMAKE_INSTALL_PREFIX=$INSTALL_DIR/cppcms \
      -DCMAKE_PREFIX_PATH="$INSTALL_DIR/pcre;$INSTALL_DIR/icu;$INSTALL_DIR/grpc" \
      -DDISABLE_SHARED=ON \
      -DDISABLE_SCGI=ON \
      -DDISABLE_TCPCACHE=ON \
      -DDISABLE_GCRYPT=ON \
      -DDISABLE_ICONV=ON
    make
    make install
  popd
fi
echo $CPPCMS_VERSION > $CPPCMS_VERSION_FILE

# cppdb
if [ $CPPDB_CHANGED -eq 1 -o ! -e $INSTALL_DIR/cppdb/lib/libcppdb.a ]; then
  rm -rf $SOURCE_DIR/cppdb
  git clone --branch v$CPPDB_VERSION --depth 1 https://github.com/melpon/cppdb.git $SOURCE_DIR/cppdb

  PATCH_DIR=`pwd`/patch
  # パッチの適用
  pushd $SOURCE_DIR/cppdb
    patch -p1 < $PATCH_DIR/004_cppdb_noshared.patch
  popd

  # ビルドとインストール
  rm -rf $BUILD_DIR/cppdb-build
  mkdir -p $BUILD_DIR/cppdb-build
  pushd $BUILD_DIR/cppdb-build
    cmake $SOURCE_DIR/cppdb \
      -DCMAKE_BUILD_TYPE=Release \
      -DCMAKE_INSTALL_PREFIX=$INSTALL_DIR/cppdb \
      -DCMAKE_PREFIX_PATH="$INSTALL_DIR/sqlite3" \
      -DDISABLE_MYSQL=ON \
      -DDISABLE_PQ=ON \
      -DDISABLE_ODBC=ON \
      -DSQLITE_BACKEND_INTERNAL=ON
    make
    make install
  popd
fi
echo $CPPDB_VERSION > $CPPDB_VERSION_FILE

# CLI11
if [ $CLI11_CHANGED -eq 1 -o  ! -e $INSTALL_DIR/CLI11/include ]; then
  rm -rf $INSTALL_DIR/CLI11
  git clone --branch v$CLI11_VERSION --depth 1 https://github.com/CLIUtils/CLI11.git $INSTALL_DIR/CLI11
fi
echo $CLI11_VERSION > $CLI11_VERSION_FILE

# spdlog
if [ $SPDLOG_CHANGED -eq 1 -o  ! -e $INSTALL_DIR/spdlog/include ]; then
  rm -rf $INSTALL_DIR/spdlog
  git clone --branch v$SPDLOG_VERSION --depth 1 https://github.com/gabime/spdlog.git $INSTALL_DIR/spdlog
fi
echo $SPDLOG_VERSION > $SPDLOG_VERSION_FILE

# ggrpc
if [ $GGRPC_CHANGED -eq 1 -o  ! -e $INSTALL_DIR/ggrpc/include/server.h ]; then
  rm -rf $INSTALL_DIR/ggrpc
  git clone --branch $GGRPC_VERSION --depth 1 https://github.com/melpon/ggrpc.git $INSTALL_DIR/ggrpc
fi
echo $GGRPC_VERSION > $GGRPC_VERSION_FILE
