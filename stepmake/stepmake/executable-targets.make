# Executable.make

# dependency list of executable:
#

default: $(EXECUTABLE)

$(EXECUTABLE): $(depth)/config.h $(O_FILES) $(outdir)/version.hh
	$(foreach a, $(MODULE_LIBS), $(MAKE) -C $(a) && ) true
	$(LD_COMMAND) $(O_FILES) $(LOADLIBES)

exe: $(EXECUTABLE)



localinstall: installexe

localuninstall: uninstallexe

installexe: all
	-$(INSTALL) -d $(bindir)
	$(foreach a, $(EXECUTABLES), \
		$(INSTALL) -m 755 $(outdir)/$(a) $(bindir)/$(a)$(DOTEXE) && \
		strip $(bindir)/$(a)$(DOTEXE) && ) true
	$(foreach a, $(SEXECUTABLES), \
		$(INSTALL) -m 755 $(outdir)/$(a) $(bindir) &&) true

uninstallexe:
	$(foreach a, $(EXECUTABLES), rm -f $(bindir)/$(a)$(DOTEXE) &&) true
	$(foreach a, $(SEXECUTABLES), rm -f $(bindir)/$(a) &&) true


# Piss off.  Use MODULE_LIBS
#
# UGH. fucks up if $(outdir) not created.
#
#%/$(outdir)/library.a:
#	$(MAKE) -C $(dir $@)/.. default
