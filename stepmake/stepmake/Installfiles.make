# Installfiles.make

localinstall: localinstall-files

localinstall-files:
	$(INSTALL) -d $(INSTALLATION_DIR)
	$(foreach i,  $(INSTALLATION_FILES),\
		$(INSTALL) -m 644 $(i) $(INSTALLATION_DIR) &&)true

localuninstall: localuninstall-files

localuninstall-files:
	$(foreach i,  $(INSTALLATION_FILES),
		rm -f $(INSTALLATION_DIR)/$(i) && ) true
	-rmdir $(INSTALLATION_DIR)
