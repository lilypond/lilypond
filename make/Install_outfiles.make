

localinstall: localinstall-files

localinstall-files: $(INSTALLATION_OUT_FILES)
	$(INSTALL) -d $(INSTALLATION_OUT_DIR)
	$(INSTALL) -m 644 $(INSTALLATION_OUT_FILES) $(INSTALLATION_OUT_DIR)

localuninstall: localuninstall-files

localuninstall-files:
	for i in $(notdir $(INSTALLATION_OUT_FILES)) ; do \
		rm -f $(INSTALLATION_OUT_DIR)/$$i; \
	done
	-rmdir $(INSTALLATION_OUT_DIR)
