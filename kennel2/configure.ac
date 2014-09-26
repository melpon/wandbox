#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.68])
AC_INIT([kennel], [0.0], [https://github.com/melpon/wandbox/issues])
AM_INIT_AUTOMAKE([-Wall])
AC_CONFIG_HEADERS([config.h])

# Checks for programs.
AC_PROG_CXX

AC_ARG_WITH(cppcms,
[AS_HELP_STRING([--with-cppcms=PATH],
                [specify prefix directory for installed CppCMS package.
                 Equivalent to --with-cppcms-include=PATH/include
                 plus --with-cppcms-lib=PATH/lib])])
AC_ARG_WITH(cppcms-include,
[AS_HELP_STRING([--with-cppcms-include=PATH],
                [specify directory for installed CppCMS include files])])
AC_ARG_WITH(cppcms-lib,
[AS_HELP_STRING([--with-cppcms-lib=PATH],
                [specify directory for the installed CppCMS library])])

cppcmslibnames="-lcppcms -lbooster"
if test "x$with_cppcms" != x; then
  cppcmsrpath="-Wl,-rpath,$with_cppcms/lib"
  cppcmslibs="-L$with_cppcms/lib"
  cppcmsinc="-I$with_cppcms/include"
fi
if test "x$with_cppcms_include" != x; then
  cppcmsinc="-I$with_cppcms_include"
fi
if test "x$with_cppcms_lib" != x; then
  cppcmsrpath="-Wl,-rpath,$with_cppcms_lib"
  cppcmslibs="-L$with_cppcms_lib"
fi

AC_ARG_WITH(cppdb,
[AS_HELP_STRING([--with-cppdb=PATH],
                [specify prefix directory for installed CppCMS package.
                 Equivalent to --with-cppdb-include=PATH/include
                 plus --with-cppdb-lib=PATH/lib])])
AC_ARG_WITH(cppdb-include,
[AS_HELP_STRING([--with-cppdb-include=PATH],
                [specify directory for installed CppCMS include files])])
AC_ARG_WITH(cppdb-lib,
[AS_HELP_STRING([--with-cppdb-lib=PATH],
                [specify directory for the installed CppCMS library])])

cppdblibnames="-lcppdb -lcppdb_sqlite3"
if test "x$with_cppdb" != x; then
  cppdbrpath="-Wl,-rpath,$with_cppdb/lib"
  cppdblibs="-L$with_cppdb/lib"
  cppdbinc="-I$with_cppdb/include"
fi
if test "x$with_cppdb_include" != x; then
  cppdbinc="-I$with_cppdb_include"
fi
if test "x$with_cppdb_lib" != x; then
  cppdbrpath="-Wl,-rpath,$with_cppdb_lib"
  cppdblibs="-L$with_cppdb_lib"
fi

CXXFLAGS="$CXXFLAGS $cppcmsinc $cppdbinc"
LIBS="$LIBS $cppcmslibs $cppcmslibnames $cppdblibnames"
LDFLAGS="$LDFLAGS $cppcmsrpath $cppdbrpath"

# AC_MSG_CHECKING([for cppcms])
# AC_LANG([C++])
# AC_TRY_LINK(
#  [
#   #include "boost/version.hpp"
#   #include "boost/system/error_code.hpp"
#  ], [
#   #if !defined(BOOST_VERSION) || BOOST_VERSION < 104700
#   choke me
#   #endif
#   boost::system::error_code ec;
#   (void)ec;
#  ],
#  [AC_MSG_RESULT([yes])],
#  [AC_MSG_RESULT([no]); AC_MSG_ERROR([cattleshed requires customized CppCMS. Try to get sources from https://github.com/melpon/cppdb])])

AC_MSG_CHECKING([for cppdb])
AC_LANG([C++])
AC_TRY_LINK(
 [
  #include <cppdb/frontend.h>
 ], [
  cppdb::session sql("sqlite3:db=:memory:");
  (void)sql;
 ],
 [AC_MSG_RESULT([yes])],
 [AC_MSG_RESULT([no]); AC_MSG_ERROR([kennel requires CppDB with SQLite3.])])

AC_CHECK_LIB([crypto], [SHA512_Init], [], [AC_MSG_ERROR([missing crypto function])])
AC_CHECK_LIB([pcre], [pcre_compile], [], [AC_MSG_ERROR([missing pcre function])])
AC_CHECK_LIB([pthread], [pthread_create], [], [AC_MSG_ERROR([missing pthread function])])
AC_CHECK_LIB([sqlite3], [sqlite3_open_v2], [], [AC_MSG_ERROR([missing sqlite3 function])])
AC_CHECK_LIB([z], [deflateReset], [], [AC_MSG_ERROR([missing z function])])
AC_CHECK_LIB([dl], [dlopen], [], [AC_MSG_ERROR([missing dl function])])

AC_CONFIG_FILES([Makefile src/Makefile])
AC_OUTPUT
