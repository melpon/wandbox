dist_pkgdata_DATA=upstart
EXTRA_DIST=upstart.in
upstart: upstart.in
	sed 's#[@]bindir[@]#$(bindir)#g' $< > $@
