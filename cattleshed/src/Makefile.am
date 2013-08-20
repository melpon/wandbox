AM_CXXFLAGS = -std=c++0x -Wall -Wextra @CXXFLAGS@
bin_PROGRAMS = cattleshed ptracer
cattleshed_SOURCES = server.cc load_config.cc quoted_printable.cc
ptracer_SOURCES = ptracer.cc load_config.cc
AM_CPPFLAGS = -DBINDIR=\"$(bindir)\" -DSYSCONFDIR=\"$(sysconfdir)\" @CPPFLAGS@
