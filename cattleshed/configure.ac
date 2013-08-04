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

boostlibnames="-lboost_system -lboost_program_options"
if test "x$with_boost" != x; then
  boostlibs="-L$with_boost/lib"
  boostinc="-I$with_boost/include"
fi
if test "x$with_boost_include" != x; then
  boostinc="-I$with_boost_include"
fi
if test "x$with_boost_lib" != x; then
  boostlibs="-L$with_boost_lib"
fi

CXXFLAGS="$CXXFLAGS $boostinc"
LIBS="$LIBS $boostlibs $boostlibnames"
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
  
# Checks for libraries.
AC_CHECK_LIB([pthread], [pthread_create])
AC_CHECK_LIB([rt], [aio_write])

# Checks for header files.
AC_CHECK_HEADERS([fcntl.h stdlib.h sys/time.h syslog.h unistd.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_C_INLINE
AC_TYPE_MODE_T
AC_TYPE_PID_T
AC_TYPE_SIZE_T

# Checks for library functions.
AC_FUNC_FORK
AC_CHECK_FUNCS([dup2 fchdir getcwd mkdir realpath strerror])

AC_CONFIG_FILES([Makefile src/Makefile test/Makefile])
AC_OUTPUT
