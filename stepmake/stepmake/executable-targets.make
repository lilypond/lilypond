default: $(EXECUTABLE)

exe: $(EXECUTABLE)

local-install: installexe

local-uninstall: uninstallexe

installexe: all
	-$(INSTALL) -d $(bindir)
	$(foreach a, $(EXECUTABLES), \
		$(INSTALL) -m 755 $(outdir)/$(a) \
		$(bindir)/$(program_prefix)$(a)$(program_suffix) && \
		strip $(bindir)/$(program_prefix)$(a)$(program_suffix) && ) true
	$(foreach a, $(SEXECUTABLES), \
		$(INSTALL) -m 755 $(outdir)/$(a) $(bindir) &&) true

uninstallexe:
	$(foreach a, $(EXECUTABLES), rm -f \
		$(bindir)/$(program_prefix)$(a)$(program_suffix) && ) true
	$(foreach a, $(SEXECUTABLES), rm -f $(bindir)/$(a) &&) true


