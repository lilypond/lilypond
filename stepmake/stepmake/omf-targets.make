
omf: $(OMF_FILES)


omf-local-install: $(OMF_FILES)
	$(foreach a, $(OMF_FILES),  $(INSTALL) $(a) $(local_package_omfdir)/$(a) && ) true

local-install: omf-local-install
