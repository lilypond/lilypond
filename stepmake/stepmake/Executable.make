# Executable.make

# dependency list of executable:
#

default: $(EXECUTABLE)

$(EXECUTABLE): $(configheader) $(O_FILES) $(outdir)/version.hh $(MODULE_LIBES)
	$(foreach a, $(MODULE_LIBS), $(MAKE) -C $a && ) true
	$(LD_COMMAND) $(O_FILES) $(LOADLIBES)

exe: $(EXECUTABLE)



localinstall: installexe

localuninstall: uninstallexe

installexe: all
	-$(INSTALL) -d $(bindir)
	$(foreach a, $(EXECUTABLES), \
		$(INSTALL) -m 755 $(outdir)/$(a)$(DOTEXE) $(bindir)/$(a)$(DOTEXE) && \
		strip $(bindir)/$(a)$(DOTEXE) && ) true
	$(foreach a, $(SEXECUTABLES), \
		$(INSTALL) -m 755 $(outdir)/$(a) $(bindir) &&) true

uninstallexe:
	$(foreach a, $(EXECUTABLES), rm -f $(bindir)/$(a)$(DOTEXE) &&) true
	$(foreach a, $(SEXECUTABLES), rm -f $(bindir)/$(a) &&) true


%/$(outdir)/library.a:
	$(MAKE) -C $(dir $@)/.. default
