default: $(EXECUTABLE)

$(EXECUTABLE): $(outdir)/config.h $(O_FILES) $(outdir)/version.hh
	$(foreach a, $(MODULE_LIBS), $(MAKE) -C $(a) && ) true
	$(LD_COMMAND) $(O_FILES) $(LOADLIBES) $(USER_LDFLAGS)

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


