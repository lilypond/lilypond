# install-targets.make

localinstall: localinstall-files localinstall-outfiles

localinstall-outfiles:

# urg, parameterise
localinstall-files:
	$(PRE_INSTALL)
	-$(INSTALL) -d $(INSTALLATION_DIR)
	$(foreach i,  $(INSTALLATION_FILES),\
		$(INSTALL) -m 644 $(i) $(INSTALLATION_DIR)/ &&)true
	$(foreach suff, $(INSTALLATION_SUFFIXES),  \
		($(INSTALL) -d $(INSTALLATION_DIR$(suff)) || true) && \
		$(foreach i, $(INSTALLATION_FILES$(suff)), \
			$(INSTALL) -m 644 $(i) $(INSTALLATION_DIR$(suff))/  && )  && ) true
	$(POST_INSTALL)

localuninstall: localuninstall-outfiles localuninstall-files 

localuninstall-outfiles:

localuninstall-files:
	rm -f $(foreach i,  $(INSTALLATION_FILES), $(INSTALLATION_DIR)/$(i))
	rm -f $(foreach suff, $(INSTALLATION_SUFFIXES),  \
		$(foreach i, $(INSTALLATION_FILES$(suff)),\
			$(INSTALLATION_DIR$(suff)/$(i))))
	-rmdir $(INSTALLATION_DIR) $(foreach suff, $(INSTALLATION_SUFFIXES), $(INSTALLATION_DIR$(suff))/)

