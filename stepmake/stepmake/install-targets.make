
local-install: local-install-files local-install-outfiles

local-install-outfiles:

# urg, parameterise
local-install-files: $(INSTALLATION_FILES)
	$(PRE_INSTALL)
	-$(INSTALL) -d $(DESTDIR)$(INSTALLATION_DIR)
	$(INSTALLPY) -m 644 $(addprefix $(src-dir)/,$(INSTALLATION_FILES)) $(DESTDIR)$(INSTALLATION_DIR)/

	$(foreach suff, $(INSTALLATION_SUFFIXES),  \
		($(INSTALL) -d $(DESTDIR)$(INSTALLATION_DIR$(suff)) || true) && \
		$(INSTALL) -m 644  $(addprefix $(src-dir)/, $(INSTALLATION_FILES$(suff))) $(DESTDIR)$(INSTALLATION_DIR$(suff))/ ) 
	$(POST_INSTALL)

local-uninstall: local-uninstall-outfiles local-uninstall-files 

local-uninstall-outfiles:

local-uninstall-files:
	rm -f $(foreach i,  $(INSTALLATION_FILES), $(DESTDIR)$(INSTALLATION_DIR)/$(i))
	rm -f $(foreach suff, $(INSTALLATION_SUFFIXES),  \
		$(foreach i, $(INSTALLATION_FILES$(suff)),\
			$(DESTDIR)$(INSTALLATION_DIR$(suff)/$(i))))
	-rmdir $(DESTDIR)$(INSTALLATION_DIR) $(foreach suff, $(INSTALLATION_SUFFIXES), $(DESTDIR)$(INSTALLATION_DIR$(suff))/)

