dist_pkgdata_DATA=upstart
dist_sysconf_DATA=cattleshed.service
EXTRA_DIST=upstart.in cattleshed.service.in
upstart: upstart.in
	sed 's#[@]bindir[@]#$(bindir)#g' $< > $@

cattleshed.service: cattleshed.service.in
	sed 's#[@]bindir[@]#$(bindir)#g' $< > $@
	sed 's#[@]prefix[@]#$(prefix)#g' -i $@
	sed 's#[@]sysconfdir[@]#$(sysconfdir)#g' -i $@
