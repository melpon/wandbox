Install
=======

NOTE: CppCMS that is used Wandbox is customized.
So you should get sources from https://github.com/melpon/cppcms to install.

\1. Install CppCMS requirements.

You can see the `document <http://cppcms.com/wikipp/en/page/cppcms_1x_build>`_.

document says:

  Mandatory Requirements:

    - Modern C++ Compiler -- GCC, MSVC 9, Intel. `See supported compilers and platforms <http://cppcms.com/wikipp/en/page/cppcms_1x_platforms>`_
    - CMake 2.6 and above, 2.8.x is recommended.
    - Zlib library
    - PCRE library.
    - Python >=2.4 (but not 3)

  Dependencies for Common Linux Distributions:

    Debian Based (Debian, Ubuntu):

      Packages: cmake libpcre3-dev zlib1g-dev libgcrypt11-dev libicu-dev python

      Getting:

      ::

        aptitude install cmake libpcre3-dev zlib1g-dev libgcrypt11-dev libicu-dev python

    RPM Based (RadHat, CentOS, Fedora, Suse):

      Packages: cmake gcc-c++ gcc make zlib-devel pcre-devel libicu-devel libgcrypt-devel

      Getting:

      ::

        yum install cmake gcc-c++ gcc make zlib-devel pcre-devel libicu-devel libgcrypt-devel

\2. Install CppCMS.

::

  git clone https://github.com/melpon/cppcms.git
  mkdir cppcms_build
  cd cppcms_build
  cmake ../cppcms/ -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=/path/to/install -DDISABLE_SHARED=ON -DDISABLE_FCGI=ON -DDISABLE_SCGI=ON -DDISABLE_ICU_LOCALE=ON -DDISABLE_TCPCACHE=ON
  make
  sudo make install

\3. Install CppDB requirements.

::

  yum install sqlite-devel
  or
  apt-get install libsqlite3-dev

NOTE: To be exact, CppDB is not required SQLite library. But, kennel require CppDB with SQLite finally.

\4. Install CppDB.

::

  git clone https://github.com/melpon/cppdb.git
  mkdir cppdb_build
  cd cppdb_build
  cmake ../cppdb/ -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=/path/to/install -DSQLITE_BACKEND_INTERNAL=ON -DDISABLE_MYSQL=ON -DDISABLE_PQ=ON -DDISABLE_ODBC=ON
  make
  make install

\5. Install kennel.

kennel requires autoconf::

  yum install autoconf

There are kennel settings (root, port, domain, etc) in ``kennel.json.in``.
You can rewrite this file before run ``./configure``.

https://github.com/melpon/wandbox/blob/master/kennel2/kennel.json.in

"application" is kennel specific settings.

Others are `CppCMS settings <http://cppcms.com/wikipp/en/page/cppcms_1x_config>`_.

And then install kennel::

  cd wandbox/kennel
  ./autogen.sh
  ./configure --prefix=/path/to/install --with-cppcms=/path/to/cppcms --with-cppdb=/path/to/cppdb
  make
  sudo make install

Run
====

::

  /path/to/install/bin/kennel -c /path/to/install/etc/kennel.json
