default: $(EXECUTABLE)

exe: $(EXECUTABLE)

local-install: installexe

local-uninstall: uninstallexe

installexe: all
	-$(INSTALL) -d $(DESTDIR)$(bindir)
	$(foreach a, $(EXECUTABLES), \
		$(INSTALL) -m 755 $(outdir)/$(a) \
		$(DESTDIR)$(bindir)/$(program_prefix)$(a)$(program_suffix) && ) true
	$(foreach a, $(SEXECUTABLES), \
		$(INSTALL) -m 755 $(outdir)/$(a) $(DESTDIR)$(bindir) &&) true

uninstallexe:
	$(foreach a, $(EXECUTABLES), rm -f \
		$(DESTDIR)$(bindir)/$(program_prefix)$(a)$(program_suffix) && ) true
	$(foreach a, $(SEXECUTABLES), rm -f $(DESTDIR)$(bindir)/$(a) &&) true


