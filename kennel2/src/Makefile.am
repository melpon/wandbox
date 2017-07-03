AM_CXXFLAGS = -std=c++0x -Wall -Wextra @CXXFLAGS@
bin_PROGRAMS = kennel
kennel_SOURCES = kennel.cpp skin_kennel.cpp
# AM_CPPFLAGS = -DBINDIR=\"$(bindir)\" -DSYSCONFDIR=\"$(sysconfdir)\" -DBOOST_SPIRIT_USE_PHOENIX_V3=1 @CPPFLAGS@

SKIN_KENNEL = \
	common.tmpl \
	root.tmpl \
	user.tmpl \
	nojs_root.tmpl \
	nojs_list.tmpl

skin_kennel.cpp: $(SKIN_KENNEL)
	@CPPCMS_TMPL_CC@ $(SKIN_KENNEL) -o $@
