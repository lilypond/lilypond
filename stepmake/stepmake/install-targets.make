# install-targets.make

localinstall: localinstall-files localinstall-outfiles

localinstall-outfiles:

# urg, parameterise
localinstall-files:
	$(PRE_INSTALL)
	-$(INSTALL) -d $(INSTALLATION_DIR)
	$(foreach i,  $(INSTALLATION_FILES),\
		$(INSTALL) -m 644 $(i) $(INSTALLATION_DIR) &&)true
	$(POST_INSTALL)
ifneq ($(strip $(INSTALLATION_FILES1)),)
	$(PRE_INSTALL1)
	-$(INSTALL) -d $(INSTALLATION_DIR1)
	$(foreach i,  $(INSTALLATION_FILES1),\
		$(INSTALL) -m 644 $(i) $(INSTALLATION_DIR1) &&)true
	$(POST_INSTALL1)
endif
ifneq ($(strip $(INSTALLATION_FILES2)),)
	$(PRE_INSTALL2)
	-$(INSTALL) -d $(INSTALLATION_DIR2)
	$(foreach i,  $(INSTALLATION_FILES2),\
		$(INSTALL) -m 644 $(i) $(INSTALLATION_DIR2) &&)true
	$(POST_INSTALL2)
endif

localuninstall: localuninstall-outfiles localuninstall-files 

localuninstall-outfiles:

localuninstall-files:
	$(foreach i,  $(INSTALLATION_FILES),\
		rm -f $(INSTALLATION_DIR)/$(i) && ) true
	-$(foreach i, $(SUBDIRS), rmdir $(INSTALLATION_DIR)/$(i); )
	-rmdir $(INSTALLATION_DIR)
ifneq ($(strip $(INSTALLATION_FILES1)),)
	$(foreach i,  $(INSTALLATION_FILES1),\
		rm -f $(INSTALLATION_DIR1)/$(i) && ) true
	-$(foreach i, $(SUBDIRS), rmdir $(INSTALLATION_DIR1)/$(i); )
	-rmdir $(INSTALLATION_DIR1)
endif
ifneq ($(strip $(INSTALLATION_FILES2)),)
	$(foreach i,  $(INSTALLATION_FILES2),\
		rm -f $(INSTALLATION_DIR2)/$(i) && ) true
	-$(foreach i, $(SUBDIRS), rmdir $(INSTALLATION_DIR2)/$(i); )
	-rmdir $(INSTALLATION_DIR2)
endif
