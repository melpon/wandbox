#!/bin/bash

BUILD_DIR="`pwd`/_build"
INSTALL_DIR="`pwd`/_install"

set -ex

mkdir -p $BUILD_DIR
mkdir -p $INSTALL_DIR

CMAKE_VERSION="3.15.3"
CMAKE_VERSION_FILE="$INSTALL_DIR/cmake.version"
CMAKE_CHANGED=0
if [ ! -e $CMAKE_VERSION_FILE -o "$CMAKE_VERSION" != "`cat $CMAKE_VERSION_FILE`" ]; then
  CMAKE_CHANGED=1
fi

ZLIB_VERSION="1.2.11"
ZLIB_VERSION_FILE="$INSTALL_DIR/zlib.version"
ZLIB_CHANGED=0
if [ ! -e $ZLIB_VERSION_FILE -o "$ZLIB_VERSION" != "`cat $ZLIB_VERSION_FILE`" ]; then
  ZLIB_CHANGED=1
fi

BOOST_VERSION="1.71.0"
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

GO_VERSION="1.13"
GO_VERSION_FILE="$INSTALL_DIR/go.version"
GO_CHANGED=0
if [ ! -e $GO_VERSION_FILE -o "$GO_VERSION" != "`cat $GO_VERSION_FILE`" ]; then
  GO_CHANGED=1
fi

BORINGSSL_VERSION="6f3e034"
BORINGSSL_VERSION_FILE="$INSTALL_DIR/boringssl.version"
BORINGSSL_CHANGED=0
if [ ! -e $BORINGSSL_VERSION_FILE -o "$BORINGSSL_VERSION" != "`cat $BORINGSSL_VERSION_FILE`" ]; then
  BORINGSSL_CHANGED=1
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

# CMake が古いとビルド出来ないので、インストール済み CMake から新しい CMake をインストールする
if [ $CMAKE_CHANGED -eq 1 -o ! -e $INSTALL_DIR/cmake/bin/cmake ]; then
  _URL=https://github.com/Kitware/CMake/releases/download/v$CMAKE_VERSION/cmake-$CMAKE_VERSION.tar.gz
  _FILE=$BUILD_DIR/cmake-$CMAKE_VERSION.tar.gz
  if [ ! -e $_FILE ]; then
    echo "file(DOWNLOAD $_URL $_FILE)" > $BUILD_DIR/tmp.cmake
    cmake -P $BUILD_DIR/tmp.cmake
    rm $BUILD_DIR/tmp.cmake
  fi

  pushd $BUILD_DIR
    rm -rf cmake-$CMAKE_VERSION
    cmake -E tar xf $_FILE
  popd

  pushd $BUILD_DIR/cmake-$CMAKE_VERSION
    ./configure --no-system-zlib --system-curl --prefix=$INSTALL_DIR/cmake
    make -j4
    make install
  popd
fi
echo $CMAKE_VERSION > $CMAKE_VERSION_FILE

export PATH=$INSTALL_DIR/cmake/bin:$PATH

# zlib
if [ $ZLIB_CHANGED -eq 1 -o ! -e $INSTALL_DIR/zlib/lib/libz.a ]; then
  _URL=https://www.zlib.net/zlib-$ZLIB_VERSION.tar.gz
  _FILE=$BUILD_DIR/zlib-$zLIB_VERSION.tar.gz
  if [ ! -e $_FILE ]; then
    echo "file(DOWNLOAD $_URL $_FILE)" > $BUILD_DIR/tmp.cmake
    cmake -P $BUILD_DIR/tmp.cmake
    rm $BUILD_DIR/tmp.cmake
  fi

  pushd $BUILD_DIR
    rm -rf zlib-$ZLIB_VERSION
    cmake -E tar xf $_FILE
  popd

  pushd $BUILD_DIR/zlib-$ZLIB_VERSION
    ./configure --static --prefix=$INSTALL_DIR/zlib
    make -j4
    make install
  popd
fi
echo $ZLIB_VERSION > $ZLIB_VERSION_FILE

# boost
if [ $BOOST_CHANGED -eq 1 -o ! -e $INSTALL_DIR/boost/lib/libboost_filesystem.a ]; then
  _VERSION_UNDERSCORE=${BOOST_VERSION//./_}
  _URL=https://dl.bintray.com/boostorg/release/${BOOST_VERSION}/source/boost_${_VERSION_UNDERSCORE}.tar.gz
  _FILE=$BUILD_DIR/boost_${_VERSION_UNDERSCORE}.tar.gz
  if [ ! -e $_FILE ]; then
    echo "file(DOWNLOAD $_URL $_FILE)" > $BUILD_DIR/tmp.cmake
    cmake -P $BUILD_DIR/tmp.cmake
    rm $BUILD_DIR/tmp.cmake
  fi
  pushd $BUILD_DIR
    rm -rf boost_${_VERSION_UNDERSCORE}
    cmake -E tar xf $_FILE

    pushd boost_${_VERSION_UNDERSCORE}
      ./bootstrap.sh
      ./b2 install --prefix=$INSTALL_DIR/boost --build-dir=build link=static --with-filesystem --with-program_options
    popd
  popd
fi
echo $BOOST_VERSION > $BOOST_VERSION_FILE

# icu
if [ $ICU_CHANGED -eq 1 -o ! -e $INSTALL_DIR/icu/lib/libicudata.a ]; then
  _VERSION_UNDERSCORE=${ICU_VERSION//./_}
  _VERSION_MINUS=${ICU_VERSION//./-}
  _URL=https://github.com/unicode-org/icu/releases/download/release-$_VERSION_MINUS/icu4c-$_VERSION_UNDERSCORE-src.tgz
  _FILE=$BUILD_DIR/icu4c-$_VERSION_UNDERSCORE-src.tgz
  if [ ! -e $_FILE ]; then
    echo "file(DOWNLOAD $_URL $_FILE)" > $BUILD_DIR/tmp.cmake
    cmake -P $BUILD_DIR/tmp.cmake
    rm $BUILD_DIR/tmp.cmake
  fi

  pushd $BUILD_DIR
    rm -rf icu
    rm -rf icu-source
    cmake -E tar xf $_FILE
    mv icu icu-source
  popd

  pushd $BUILD_DIR/icu-source/source
    ./configure \
      --prefix=$INSTALL_DIR/icu \
      --disable-tests \
      --disable-samples \
      --disable-shared \
      --enable-static
    make -j4
    make install
  popd
fi
echo $ICU_VERSION > $ICU_VERSION_FILE

# cppcms
if [ $CPPCMS_CHANGED -eq 1 -o ! -e $INSTALL_DIR/cppcms/lib/libcppcms.a ]; then
  rm -rf $BUILD_DIR/cppcms-source
  git clone --branch v$CPPCMS_VERSION --depth 1 https://github.com/artyom-beilis/cppcms.git $BUILD_DIR/cppcms-source

  PATCH_DIR=`pwd`/patch
  # パッチの適用
  pushd $BUILD_DIR/cppcms-source
    patch -p1 < $PATCH_DIR/001_http_protocol.patch
    patch -p1 < $PATCH_DIR/002_ignore_http_header_comments.patch
    patch -p1 < $PATCH_DIR/003_cxx11_notest.patch
  popd

  # ビルドとインストール
  mkdir -p $BUILD_DIR/cppcms-build
  pushd $BUILD_DIR/cppcms-build
    cmake $BUILD_DIR/cppcms-source \
      -DCMAKE_BUILD_TYPE=Release \
      -DCMAKE_INSTALL_PREFIX=$INSTALL_DIR/cppcms \
      -DCMAKE_PREFIX_PATH="$INSTALL_DIR/zlib;$INSTALL_DIR/icu;$INSTALL_DIR/boringssl" \
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
  rm -rf $BUILD_DIR/cppdb-source
  git clone --branch v$CPPDB_VERSION --depth 1 https://github.com/melpon/cppdb.git $BUILD_DIR/cppdb-source

  # ビルドとインストール
  mkdir -p $BUILD_DIR/cppdb-build
  pushd $BUILD_DIR/cppdb-build
    cmake $BUILD_DIR/cppdb-source \
      -DCMAKE_BUILD_TYPE=Release \
      -DCMAKE_INSTALL_PREFIX=$INSTALL_DIR/cppdb \
      -DDISABLE_MYSQL=ON \
      -DDISABLE_PQ=ON \
      -DDISABLE_ODBC=ON \
      -DSQLITE_BACKEND_INTERNAL=ON
    make
    make install
  popd
fi
echo $CPPDB_VERSION > $CPPDB_VERSION_FILE

# Go
if [ $GO_CHANGED -eq 1 -o ! -e $INSTALL_DIR/go/bin/go ]; then
  # Bootstrap
  _URL=https://dl.google.com/go/go1.4-bootstrap-20171003.tar.gz
  _FILE=$BUILD_DIR/go1.4-bootstrap-20171003.tar.gz
  if [ ! -e $_FILE ]; then
    echo "file(DOWNLOAD $_URL $_FILE)" > $BUILD_DIR/tmp.cmake
    cmake -P $BUILD_DIR/tmp.cmake
    rm $BUILD_DIR/tmp.cmake
  fi

  pushd $BUILD_DIR
    rm -rf go
    rm -rf go-bootstrap
    cmake -E tar xf $_FILE
    mv go go-bootstrap
  popd

  pushd $BUILD_DIR/go-bootstrap/src
    CGO_ENABLED=0 ./make.bash
  popd

  # 本体
  _URL=https://github.com/golang/go/archive/go$GO_VERSION.tar.gz
  _FILE=$BUILD_DIR/go$GO_VERSION.tar.gz
  if [ ! -e $_FILE ]; then
    echo "file(DOWNLOAD $_URL $_FILE)" > $BUILD_DIR/tmp.cmake
    cmake -P $BUILD_DIR/tmp.cmake
    rm $BUILD_DIR/tmp.cmake
  fi

  pushd $BUILD_DIR
    rm -rf go-go$GO_VERSION
    rm -rf $INSTALL_DIR/go
    cmake -E tar xf $_FILE
    mv go-go$GO_VERSION $INSTALL_DIR/go
  popd

  pushd $INSTALL_DIR/go/src
    GOROOT_BOOTSTRAP=$BUILD_DIR/go-bootstrap ./make.bash
  popd
fi
echo $GO_VERSION > $GO_VERSION_FILE

# boringssl
if [ $BORINGSSL_CHANGED -eq 1 -o ! -e $INSTALL_DIR/boringssl/lib/libcrypto.a ]; then
  rm -rf $BUILD_DIR/boringssl-source
  git clone https://boringssl.googlesource.com/boringssl $BUILD_DIR/boringssl-source
  pushd $BUILD_DIR/boringssl-source
    git reset --hard "$BORINGSSL_VERSION"
  popd

  mkdir -p $BUILD_DIR/boringssl-build
  pushd $BUILD_DIR/boringssl-build
    cmake $BUILD_DIR/boringssl-source \
      -DCMAKE_BUILD_TYPE=Release \
      -DCMAKE_INSTALL_PREFIX=$INSTALL_DIR/boringssl \
      -DGO_EXECUTABLE=$INSTALL_DIR/go/bin/go
    make -j4
    # make install はインストールするものが無いって言われるので
    # 手動でインストールする
    mkdir -p $INSTALL_DIR/boringssl/lib
    cp ssl/libssl.a crypto/libcrypto.a $INSTALL_DIR/boringssl/lib
    mkdir -p $INSTALL_DIR/boringssl/include
    rm -rf $INSTALL_DIR/boringssl/include/openssl
    cp -r $BUILD_DIR/boringssl-source/include/openssl $INSTALL_DIR/boringssl/include/openssl
  popd
fi
echo $BORINGSSL_VERSION > $BORINGSSL_VERSION_FILE

# pcre
if [ $PCRE_CHANGED -eq 1 -o ! -e $INSTALL_DIR/pcre/lib/libpcre.a ]; then
  _URL=https://ftp.pcre.org/pub/pcre/pcre-$PCRE_VERSION.zip
  _FILE=$BUILD_DIR/pcre-$PCRE_VERSION.zip
  if [ ! -e $_FILE ]; then
    echo "file(DOWNLOAD $_URL $_FILE)" > $BUILD_DIR/tmp.cmake
    cmake -P $BUILD_DIR/tmp.cmake
    rm $BUILD_DIR/tmp.cmake
  fi

  pushd $BUILD_DIR
    rm -rf pcre-$PCRE_VERSION
    cmake -E tar xf $_FILE
  popd

  rm -rf $BUILD_DIR/pcre-build
  mkdir -p $BUILD_DIR/pcre-build
  pushd $BUILD_DIR/pcre-build
    cmake $BUILD_DIR/pcre-$PCRE_VERSION \
      -DCMAKE_BUILD_TYPE=Release \
      -DCMAKE_INSTALL_PREFIX=$INSTALL_DIR/pcre \
      -DPCRE_SUPPORT_UTF=ON \
      -DPCRE_BUILD_PCREGREP=OFF \
      -DPCRE_BUILD_TESTS=OFF
    make -j4
    make install
  popd
fi
echo $PCRE_VERSION > $PCRE_VERSION_FILE

# curl
if [ $CURL_CHANGED -eq 1 -o ! -e $INSTALL_DIR/curl/lib/libcurl.a ]; then
  _VERSION_UNDERSCORE=${CURL_VERSION//./_}
  _URL=https://github.com/curl/curl/releases/download/curl-${_VERSION_UNDERSCORE}/curl-$CURL_VERSION.tar.gz
  _FILE=$BUILD_DIR/curl-$CURL_VERSION.tar.gz
  if [ ! -e $_FILE ]; then
    echo "file(DOWNLOAD $_URL $_FILE)" > $BUILD_DIR/tmp.cmake
    cmake -P $BUILD_DIR/tmp.cmake
    rm $BUILD_DIR/tmp.cmake
  fi

  pushd $BUILD_DIR
    rm -rf curl-$CURL_VERSION
    cmake -E tar xf $_FILE
  popd

  pushd $BUILD_DIR/curl-$CURL_VERSION
    ./configure \
      --prefix=$INSTALL_DIR/curl \
      --disable-shared \
      --disable-ldap \
      --with-ssl=$INSTALL_DIR/boringssl \
      --with-zlib=$INSTALL_DIR/zlib \
      --without-librtmp
    make -j4
    make install
  popd
fi
echo $CURL_VERSION > $CURL_VERSION_FILE

# sqlite3
if [ $SQLITE3_CHANGED -eq 1 -o ! -e $INSTALL_DIR/sqlite3/lib/libsqlite3.a ]; then
  _URL=https://www.sqlite.org/2019/sqlite-autoconf-$SQLITE3_VERSION_NUMBER.tar.gz
  _FILE=$BUILD_DIR/sqlite-autoconf-$SQLITE3_VERSION_NUMBER.tar.gz
  if [ ! -e $_FILE ]; then
    echo "file(DOWNLOAD $_URL $_FILE)" > $BUILD_DIR/tmp.cmake
    cmake -P $BUILD_DIR/tmp.cmake
    rm $BUILD_DIR/tmp.cmake
  fi

  pushd $BUILD_DIR
    rm -rf sqlite-autoconf-$SQLITE3_VERSION_NUMBER
    cmake -E tar xf $_FILE
  popd

  pushd $BUILD_DIR/sqlite-autoconf-$SQLITE3_VERSION_NUMBER
    ./configure \
      --prefix=$INSTALL_DIR/sqlite3 \
      --disable-shared \
      --enable-static
    make -j4
    make install
  popd
fi
echo $SQLITE3_VERSION > $SQLITE3_VERSION_FILE
