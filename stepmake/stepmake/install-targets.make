
local-install: local-install-files local-install-outfiles

local-install-outfiles:

# urg, parameterise
local-install-files: $(INSTALLATION_FILES)
	$(PRE_INSTALL)
	-$(INSTALL) -d $(INSTALLATION_DIR)
	for i in $(INSTALLATION_FILES); do \
		$(INSTALL) -m 644 $$i $(INSTALLATION_DIR)/ ; done
	$(foreach suff, $(INSTALLATION_SUFFIXES),  \
		($(INSTALL) -d $(INSTALLATION_DIR$(suff)) || true) && \
		for i in $(INSTALLATION_FILES$(suff)); do \
			$(INSTALL) -m 644 $$i $(INSTALLATION_DIR$(suff))/; done )
	$(POST_INSTALL)

local-uninstall: local-uninstall-outfiles local-uninstall-files 

local-uninstall-outfiles:

local-uninstall-files:
	rm -f $(foreach i,  $(INSTALLATION_FILES), $(INSTALLATION_DIR)/$(i))
	rm -f $(foreach suff, $(INSTALLATION_SUFFIXES),  \
		$(foreach i, $(INSTALLATION_FILES$(suff)),\
			$(INSTALLATION_DIR$(suff)/$(i))))
	-rmdir $(INSTALLATION_DIR) $(foreach suff, $(INSTALLATION_SUFFIXES), $(INSTALLATION_DIR$(suff))/)

