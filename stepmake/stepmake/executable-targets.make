default: $(EXECUTABLE)

exe: $(EXECUTABLE)

local-install: installexe

local-uninstall: uninstallexe

installexe: all
	-$(INSTALLPY) -d $(DESTDIR)$(bindir)
	$(foreach a, $(EXECUTABLES), \
		$(INSTALLPY) -m 755 $(outdir)/$(a) \
		$(DESTDIR)$(bindir)/$(program_prefix)$(a)$(program_suffix) && ) true
	$(INSTALLPY) -c -m 755 $(addprefix $(outdir)/, $(SEXECUTABLES)) $(DESTDIR)$(bindir)

uninstallexe:
	$(foreach a, $(EXECUTABLES), rm -f \
		$(DESTDIR)$(bindir)/$(program_prefix)$(a)$(program_suffix) && ) true
	$(foreach a, $(SEXECUTABLES), rm -f $(DESTDIR)$(bindir)/$(a) &&) true


