
omf: $(OMF_FILES)


omf-local-install: $(OMF_FILES)
	$(foreach a, $(OMF_FILES), $(INSTALL) -m 644 $(a) $(local_package_omfdir)/$(notdir $(a)) && ) true
	@echo "Run scrollkeeper-update to register newly installed OMF files."

local-install: omf-local-install
