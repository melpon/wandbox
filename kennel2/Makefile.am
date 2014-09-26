SUBDIRS = src
dist_sysconf_DATA = kennel.json
staticdir = @STATIC_DIR@
EXTRA_DIST = kennel.json.in
kennel.json: kennel.json.in
	sed 's#[@]localstatedir[@]#$(localstatedir)#g' $< > $@.tmp
	sed 's#[@]staticdir[@]#$(staticdir)#g' $@.tmp > $@
	rm $@.tmp

install-data-local:
	cd static; find . | while read file; do if [ -d "$$file" -a -L "$$file" ]; then find "$$file/" -type f | while read file2; do $(install_sh_DATA) "$$file2" "$(DESTDIR)$(staticdir)/$$file2"; done; elif [ -f "$$file" ]; then $(install_sh_DATA) "$$file" "$(DESTDIR)$(staticdir)/$$file"; fi; done
	$(mkinstalldirs) $(DESTDIR)$(localstatedir)
