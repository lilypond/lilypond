
local-install: local-install-files local-install-outfiles

local-install-files:

# urg, parameterise
local-install-outfiles: $(INSTALLATION_OUT_FILES) $(foreach suff, $(INSTALLATION_OUT_SUFFIXES), $(INSTALLATION_OUT_FILES$(suff)))
	$(if $(INSTALLATION_OUT_DIR),\
		($(INSTALLPY) -d $(DESTDIR)$(INSTALLATION_OUT_DIR) || true) \
		 && $(INSTALLPY) -c -m 644 $(INSTALLATION_OUT_FILES) $(DESTDIR)$(INSTALLATION_OUT_DIR)/, true)
	$(foreach suff, $(INSTALLATION_OUT_SUFFIXES),  \
		($(INSTALLPY) -d $(DESTDIR)$(INSTALLATION_OUT_DIR$(suff))/ || true) && \
		$(INSTALLPY) -c -m 644 $(INSTALLATION_OUT_FILES$(suff)) $(DESTDIR)$(INSTALLATION_OUT_DIR$(suff))/ && ) true

local-uninstall: local-uninstall-outfiles local-uninstall-files

local-uninstall-files:

local-uninstall-outfiles:
	rm -f $(foreach  i, $(notdir $(INSTALLATION_OUT_FILES)), \
		$(DESTDIR)$(INSTALLATION_OUT_DIR)/$(i))
	rm -f $(foreach suff, $(INSTALLATION_OUT_SUFFIXES),  \
		$(foreach i, $(notdir $(INSTALLATION_OUT_FILES$(suff))), \
			$(DESTDIR)$(INSTALLATION_OUT_DIR$(suff))/$(i)))
	-rmdir -p $(DESTDIR)$(INSTALLATION_OUT_DIR) $(foreach suff, $(INSTALLATION_OUT_SUFFIXES), $(DESTDIR)$(INSTALLATION_OUT_DIR$(suff)))
