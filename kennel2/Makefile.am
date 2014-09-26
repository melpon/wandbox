SUBDIRS = src
dist_sysconf_DATA = kennel.json
EXTRA_DIST = kennel.json.in
kennel.json: kennel.json.in
	sed 's#[@]localstatedir[@]#$(localstatedir)#g' $< > $@.tmp
	sed 's#[@]sysconfdir[@]#$(sysconfdir)#g' $@.tmp > $@
	rm $@.tmp
