AM_CXXFLAGS = -std=c++0x
bin_PROGRAMS = server.exe ptracer.exe
server_exe_SOURCES = server.cc load_config.cc
ptracer_exe_SOURCES = ptracer.cc load_config.cc
AM_CPPFLAGS = -DBINDIR=\"$(bindir)\" -DDATADIR=\"$(pkgdatadir)\" @CPPFLAGS@
