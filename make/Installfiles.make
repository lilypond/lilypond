


localinstall: localinstall-files

localinstall-files:
	$(INSTALL) -d $(INSTALLATION_DIR)
	$(INSTALL) -m 644 $(INSTALLATION_FILES) $(INSTALLATION_DIR)

localuninstall: localuninstall-files

localuninstall-files:
	for i in $(INSTALLATION_FILES) ; do \
		rm -f $(INSTALLATION_DIR)/$$i; \
	done
	-rmdir $(INSTALLATION_DIR)
