localinstall: installexe

localuninstall: uninstallexe

installexe: all
	$(INSTALL) -d $(bindir)
	for a in $(EXECUTABLES); do \
		$(INSTALL) -m 755 $(outdir)/$$a $(bindir); \
	done	

uninstallexe:
	for a in $(EXECUTABLES); do \
		rm -f $(bindir)/$$a; \
	done	

