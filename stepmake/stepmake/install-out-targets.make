# install-out-targets.make

localinstall: localinstall-files localinstall-outfiles

localinstall-files:

# urg, parameterise
localinstall-outfiles: $(INSTALLATION_OUT_FILES) $(INSTALLATION_OUT_FILES1) $(INSTALLATION_OUT_FILES2)
	-$(INSTALL) -d $(INSTALLATION_OUT_DIR)
	$(foreach i, $(INSTALLATION_OUT_FILES), \
		$(INSTALL) -m 644 $(i) $(INSTALLATION_OUT_DIR) && )true
ifneq ($(strip $(INSTALLATION_OUT_FILES1)),)
	-$(INSTALL) -d $(INSTALLATION_OUT_DIR1)
	$(foreach i, $(INSTALLATION_OUT_FILES1), \
		$(INSTALL) -m 644 $(i) $(INSTALLATION_OUT_DIR1) && )true
endif
ifneq ($(strip $(INSTALLATION_OUT_FILES2)),)
	-$(INSTALL) -d $(INSTALLATION_OUT_DIR2)
	$(foreach i, $(INSTALLATION_OUT_FILES2), \
		$(INSTALL) -m 644 $(i) $(INSTALLATION_OUT_DIR2) && )true
endif

localuninstall: localuninstall-outfiles localuninstall-files 

localuninstall-files:

localuninstall-outfiles:
	$(foreach  i, $(notdir $(INSTALLATION_OUT_FILES)), \
		rm -f $(INSTALLATION_OUT_DIR)/$(i) && ) true
	-rmdir $(INSTALLATION_OUT_DIR)
ifneq ($(strip $(INSTALLATION_OUT_FILES1)),)
	$(foreach  i, $(notdir $(INSTALLATION_OUT_FILES1)), \
		rm -f $(INSTALLATION_OUT_DIR1)/$(i) && ) true
	-rmdir $(INSTALLATION_OUT_DIR1)
endif
ifneq ($(strip $(INSTALLATION_OUT_FILES2)),)
	$(foreach  i, $(notdir $(INSTALLATION_OUT_FILES2)), \
		rm -f $(INSTALLATION_OUT_DIR2)/$(i) && ) true
	-rmdir $(INSTALLATION_OUT_DIR2)
endif
#	-(cd $(INSTALLATION_OUT_DIR)/..; dir=`dirname \`pwd\`` cd ..; rmdir $$dir)
