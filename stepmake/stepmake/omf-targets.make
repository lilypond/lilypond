
omf: $(OMF_FILES)

local-install: omf-local-install

omf-local-install: $(OMF_FILES)
	-$(INSTALL) -d $(local_package_omfdir)
	$(foreach a, $(OMF_FILES), $(INSTALL) -m 644 $(a) $(local_package_omfdir)/$(notdir $(a)) && ) true
	@echo "Run scrollkeeper-update to register newly installed OMF files."

local-uninstall: omf-local-uninstall

omf-local-uninstall:
	rm -f $(foreach i,  $(OMF_FILES), $(local_package_omfdir)/$(i))
	-rmdir $(local_package_omfdir)

