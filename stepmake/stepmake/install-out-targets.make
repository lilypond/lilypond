# install-out-targets.make

local-install: local-install-files local-install-outfiles

local-install-files:

# urg, parameterise
local-install-outfiles: $(INSTALLATION_OUT_FILES) $(foreach suff, $(INSTALLATION_OUT_SUFFIXES), $(INSTALLATION_OUT_FILES$(suff)))
	-$(INSTALL) -d $(INSTALLATION_OUT_DIR)
	$(foreach i, $(INSTALLATION_OUT_FILES), \
		$(INSTALL) -m 644 $(i) $(INSTALLATION_OUT_DIR)/ && ) true
	$(foreach suff, $(INSTALLATION_OUT_SUFFIXES),  \
		($(INSTALL) -d $(INSTALLATION_OUT_DIR$(suff))/ || true) && \
		$(foreach i, $(INSTALLATION_OUT_FILES$(suff)), \
			$(INSTALL) -m 644 $(i) $(INSTALLATION_OUT_DIR$(suff))/ && ) true && ) true


local-uninstall: local-uninstall-outfiles local-uninstall-files 

local-uninstall-files:

local-uninstall-outfiles:
	rm -f $(foreach  i, $(notdir $(INSTALLATION_OUT_FILES)), \
		$(INSTALLATION_OUT_DIR)/$(i))
	rm -f $(foreach suff, $(INSTALLATION_OUT_SUFFIXES),  \
		$(foreach i, $(INSTALLATION_OUT_FILES$(suff)), \
			$(INSTALLATION_OUT_DIR$(suff))/$(i)))
	-rmdir $(INSTALLATION_OUT_DIR) $(foreach suff, $(INSTALLATION_OUT_SUFFIXES), $(INSTALLATION_OUT_DIR$(suff)))
