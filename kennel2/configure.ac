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
                 Equivalent to --with-cppcms-bin=PATH/bin
                 plus --with-cppcms-include=PATH/include
                 plus --with-cppcms-lib=PATH/lib])])
AC_ARG_WITH(cppcms-bin,
[AS_HELP_STRING([--with-cppcms-bin=PATH],
                [specify directory for installed CppCMS binary])])
AC_ARG_WITH(cppcms-include,
[AS_HELP_STRING([--with-cppcms-include=PATH],
                [specify directory for installed CppCMS include files])])
AC_ARG_WITH(cppcms-lib,
[AS_HELP_STRING([--with-cppcms-lib=PATH],
                [specify directory for the installed CppCMS library])])

cppcmslibnames="-lcppcms -lbooster"
if test "x$with_cppcms" != x; then
  cppcmstmplcc="$with_cppcms/bin/cppcms_tmpl_cc"
  cppcmsrpath="-Wl,-rpath,$with_cppcms/lib"
  cppcmslibs="-L$with_cppcms/lib"
  cppcmsinc="-I$with_cppcms/include"
fi
if test "x$with_cppcms_bin" != x; then
  cppcmstmplcc="$with_cppcms_bin/cppcms_tmpl_cc"
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

cppdblibnames="-Wl,-Bstatic -lcppdb -Wl,-Bdynamic"
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

AC_CHECK_LIB([curl], [curl_easy_init], [], [AC_MSG_ERROR([missing curl function])])
AC_CHECK_LIB([pcre], [pcre_compile], [], [AC_MSG_ERROR([missing pcre function])])
AC_CHECK_LIB([pthread], [pthread_create], [], [AC_MSG_ERROR([missing pthread function])])
AC_CHECK_LIB([sqlite3], [sqlite3_open_v2], [], [AC_MSG_ERROR([missing sqlite3 function])])
AC_CHECK_LIB([z], [deflateReset], [], [AC_MSG_ERROR([missing z function])])
AC_CHECK_LIB([dl], [dlopen], [], [AC_MSG_ERROR([missing dl function])])

CXXFLAGS="$CXXFLAGS $cppcmsinc $cppdbinc"
CRYPT="-lgcrypt"
DEFAULT_LIBS="$LIBS $cppcmslibs $cppdblibs $cppcmslibnames $cppdblibnames -Wl,-Bstatic -lsqlite3 -Wl,-Bdynamic -lpcre -lz -lpthread -ldl -lcurl"
LIBS="$DEFAULT_LIBS"
LDFLAGS="$LDFLAGS $cppcmsrpath $cppdbrpath"

CPPCMS_TMPL_CC="$cppcmstmplcc"
AC_SUBST(CPPCMS_TMPL_CC)

STATIC_DIR="$sysconfdir/static"
AC_SUBST(STATIC_DIR)

AC_ARG_WITH(databasedir,
[AS_HELP_STRING([--with-databasedir=PATH],
                [specify where pemlink database is stored in (default: $localstatedir/lib/kennel)])])
if test "x$with_databasedir" != x; then
  DATABASE_DIR="$with_databasedir"
else
  DATABASE_DIR="$localstatedir/lib/kennel"
fi
AC_SUBST(DATABASE_DIR)

AC_ARG_WITH(sponsors,
[AS_HELP_STRING([--with-sponsors=PATH],
                [specify sponsors JSON file (default: "")])])
if test "x$with_sponsors" != x; then
  SPONSORS_FILE="$with_sponsors"
else
  SPONSORS_FILE=""
fi
AC_SUBST(SPONSORS_FILE)

AC_ARG_WITH(googleanalytics,
[AS_HELP_STRING([--with-googleanalytics=TrackingID],
                [specify Google Analytics Tracking ID (default: "")])])
if test "x$with_googleanalytics" != x; then
  GOOGLEANALYTICS="$with_googleanalytics"
else
  GOOGLEANALYTICS=""
fi
AC_SUBST(GOOGLEANALYTICS)

AC_ARG_WITH(githubclient,
[AS_HELP_STRING([--with-githubclient=Github Client ID],
                [specify Github Client ID (default: "")])])
if test "x$with_githubclient" != x; then
  GITHUBCLIENT="$with_githubclient"
else
  GITHUBCLIENT=""
fi
AC_SUBST(GITHUBCLIENT)

AC_MSG_CHECKING([for cppcms])
AC_LANG([C++])
AC_TRY_COMPILE(
 [
  #include <cppcms/http_response.h>
 ], [
  void (cppcms::http::response::*x)(int, int) = &cppcms::http::response::protocol;
  (void)x;
 ],
 [AC_MSG_RESULT([yes])],
 [AC_MSG_RESULT([no]); AC_MSG_ERROR([kennel requires customized CppCMS. Try to get sources from https://github.com/melpon/cppcms])])

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

CRYPT="-lgcrypt"
LIBS="$DEFAULT_LIBS $CRYPT"
AC_MSG_CHECKING([for gcrypt])
AC_LANG([C++])
AC_TRY_LINK(
 [
  #include <cppcms/crypto.h>
 ], [
  cppcms::crypto::key key;
  (void)key;
 ],
 [AC_MSG_RESULT([yes])],
 [AC_MSG_RESULT([no])
  CRYPT="-lcrypto"
  LIBS="$DEFAULT_LIBS $CRYPT"
  AC_MSG_CHECKING([for OpenSSL])
  AC_LANG([C++])
  AC_TRY_LINK(
   [
    #include <cppcms/crypto.h>
   ], [
    cppcms::crypto::key key;
   ],
   [AC_MSG_RESULT([yes])],
   [AC_MSG_RESULT([no]); AC_MSG_ERROR([missing gcrypt or OpenSSL library])])])

AC_MSG_CHECKING([for tm_gmtoff])
AC_TRY_COMPILE([
  #include <time.h>
], [
  struct tm tm;
  tm.tm_gmtoff = 1;
],
[AC_MSG_RESULT([yes])],
[AC_MSG_RESULT([no]); AC_MSG_ERROR([no tm_gmtoff in struct tm])])

AC_CONFIG_FILES([Makefile src/Makefile])
AC_OUTPUT
