
omf: $(OMF_FILES)

omf-local-install: $(OMF_FILES)
	-$(INSTALLPY) -d $(DESTDIR)$(local_package_omfdir)
	$(foreach a, $(OMF_FILES), $(INSTALLPY) -m 644 $(a) $(DESTDIR)$(local_package_omfdir)/$(notdir $(a)) && ) true
	@echo "Run scrollkeeper-update to register newly installed OMF files."

ifeq ($(out),www)
local-uninstall: omf-local-uninstall
endif

omf-local-uninstall:
	rm -f $(foreach i, $(OMF_FILES), $(DESTDIR)$(local_package_omfdir)/$(notdir $(i)))
	-rmdir $(DESTDIR)$(local_package_omfdir)

