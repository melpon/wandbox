#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.68])
AC_INIT([cattleshed], [0.0], [https://github.com/melpon/wandbox/issues])
AM_INIT_AUTOMAKE([-Wall])
AC_CONFIG_HEADERS([config.h])

# Checks for programs.
AC_PROG_CXX
AC_ARG_WITH(boost,
[AS_HELP_STRING([--with-boost=PATH],
                [specify prefix directory for installed Boost package.
                 Equivalent to --with-boost-include=PATH/include
                 plus --with-boost-lib=PATH/lib])])
AC_ARG_WITH(boost-include,
[AS_HELP_STRING([--with-boost-include=PATH],
                [specify directory for installed Boost include files])])
AC_ARG_WITH(boost-lib,
[AS_HELP_STRING([--with-boost-lib=PATH],
                [specify directory for the installed Boost library])])

boostlibnames="-Wl,-Bstatic -lboost_system -lboost_program_options -Wl,-Bdynamic"
if test "x$with_boost" != x; then
  boostrpath="-Wl,-rpath,$with_boost/lib"
  boostlibs="-L$with_boost/lib"
  boostinc="-I$with_boost/include"
fi
if test "x$with_boost_include" != x; then
  boostinc="-I$with_boost_include"
fi
if test "x$with_boost_lib" != x; then
  boostrpath="-Wl,-rpath,$with_boost_lib"
  boostlibs="-L$with_boost_lib"
fi

CXXFLAGS="$CXXFLAGS $boostinc"
LIBS="$LIBS $boostlibs $boostlibnames"
LDFLAGS="$LDFLAGS $boostrpath"
AC_MSG_CHECKING([for boost])
AC_LANG([C++])
AC_TRY_LINK(
 [
  #include "boost/version.hpp"
  #include "boost/system/error_code.hpp"
 ], [
  #if !defined(BOOST_VERSION) || BOOST_VERSION < 104700
  choke me
  #endif
  boost::system::error_code ec;
  (void)ec;
 ],
 [AC_MSG_RESULT([yes])],
 [AC_MSG_RESULT([no]); AC_MSG_ERROR([cattleshed requires Boost >= 1.47])])

setcap_cmd=setcap
AC_ARG_ENABLE([install-setcap],
              [perform setcap(1) by 'make install'],
              [if test "x$enableval" = xno; then setcap_cmd=true; fi])
AC_SUBST([setcap_cmd])

AC_CHECK_LIB([pthread], [pthread_create], [], [AC_MSG_ERROR([missing pthread function])])
AC_CHECK_LIB([rt], [aio_write], [], [AC_MSG_ERROR([missing aio function])])
AC_CHECK_LIB([cap], [cap_set_proc], [], [AC_MSG_ERROR([missing capability function])])

AC_CHECK_HEADER([sys/capability.h], [], [AC_MSG_ERROR([missing libcap-dev])])

AC_CONFIG_FILES([Makefile src/Makefile test/Makefile scripts/Makefile])
AC_OUTPUT
