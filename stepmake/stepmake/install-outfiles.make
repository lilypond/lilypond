# Initial_outfiles.make

localinstall: localinstall-files

localinstall-files: $(INSTALLATION_OUT_FILES)
	-$(INSTALL) -d $(INSTALLATION_OUT_DIR)
	$(foreach i, $(INSTALLATION_OUT_FILES), \
		$(INSTALL) -m 644 $(i) $(INSTALLATION_OUT_DIR) && )true

localuninstall: localuninstall-files

localuninstall-files:
	$(foreach  i, $(notdir $(INSTALLATION_OUT_FILES)), \
		rm -f $(INSTALLATION_OUT_DIR)/$(i) && ) true
	-rmdir $(INSTALLATION_OUT_DIR)
