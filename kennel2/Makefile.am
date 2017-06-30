SUBDIRS = src
dist_sysconf_DATA = kennel.json sponsors.json
staticdir = @STATIC_DIR@
databasedir = @DATABASE_DIR@
sponsorsfile = @SPONSORS_FILE@
googleanalytics = @GOOGLEANALYTICS@
githubclient = @GITHUBCLIENT@
EXTRA_DIST = kennel.json.in
kennel.json: kennel.json.in
	sed 's#[@]databasedir[@]#$(databasedir)#g' $< > $@
	sed -i 's#[@]staticdir[@]#$(staticdir)#g' $@
	sed -i 's#[@]sponsorsfile[@]#$(sponsorsfile)#g' $@
	sed -i 's#[@]googleanalytics[@]#$(googleanalytics)#g' $@
	sed -i 's#[@]githubclient[@]#$(githubclient)#g' $@

install-data-local:
	$(mkinstalldirs) "$(DESTDIR)$(staticdir)"
	rsync -rLptgok static/ "$(DESTDIR)$(staticdir)/"
	$(mkinstalldirs) -m 755 $(DESTDIR)$(databasedir)
