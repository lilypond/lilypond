


localinstall: localinstall-files

localinstall-files: $(INSTALLATION_OUT_FILES)
	$(INSTALL) -d $(INSTALLATION_DIR)
	$(INSTALL) -m 644 $(INSTALLATION_OUT_FILES) $(INSTALLATION_DIR)

localuninstall: localuninstall-files

localuninstall-files:
	for i in $(notdir $(INSTALLATION_OUT_FILES)) ; do \
		rm -f $(INSTALLATION_DIR)/$$i; \
	done
	-rmdir $(INSTALLATION_DIR)
