SUBDIRS = src scripts test
TESTS = test/jail.sh
dist_sysconf_DATA = cattleshed.conf
sysconfddir = $(sysconfdir)/cattleshed.conf.d
dist_sysconfd_DATA = compilers.default
EXTRA_DIST = cattleshed.conf.in
cattleshed.conf: cattleshed.conf.in
	sed 's#[@]bindir[@]#$(bindir)#g' $< > $@
