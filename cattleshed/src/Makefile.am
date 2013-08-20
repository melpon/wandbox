AM_CXXFLAGS = -std=c++0x -Wall -Wextra @CXXFLAGS@
bin_PROGRAMS = cattleshed cattlegrid prlimit
cattleshed_SOURCES = server.cc load_config.cc quoted_printable.cc
cattlegrid_SOURCES = jail.cc
prlimit_SOURCES = prlimit.cc
AM_CPPFLAGS = -DBINDIR=\"$(bindir)\" -DSYSCONFDIR=\"$(sysconfdir)\" -DBOOST_SPIRIT_USE_PHOENIX_V3=1 @CPPFLAGS@

install-exec-hook:
	setcap cap_sys_admin,cap_sys_chroot,cap_mknod=p  $(DESTDIR)$(bindir)/cattlegrid
