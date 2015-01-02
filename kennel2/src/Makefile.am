AM_CXXFLAGS = -std=c++0x -Wall -Wextra @CXXFLAGS@
bin_PROGRAMS = kennel
kennel_SOURCES = kennel.cpp root.cpp
# AM_CPPFLAGS = -DBINDIR=\"$(bindir)\" -DSYSCONFDIR=\"$(sysconfdir)\" -DBOOST_SPIRIT_USE_PHOENIX_V3=1 @CPPFLAGS@

.tmpl.cpp:
	@CPPCMS_TMPL_CC@ $< -o $@
