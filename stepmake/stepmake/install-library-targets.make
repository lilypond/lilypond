
localinstall: $(LIBRARY)
ifneq ($(strip $(INSTALL_HEADERS)),)
	$(INSTALL) -d $(includedir)
	$(INSTALL) $(INSTALL_HEADERS) $(includedir)
endif
ifeq ($(LIB_SUFFIX),.so)
	$(INSTALL) -d $(libdir)
	$(INSTALL) $(LIBRARY) $(libdir)/$(INSTALL_LIBRARY).$(VERSION)
	ln -s $(LIB_PREFIX)intl$(LIB_SUFFIX).$(VERSION) $(libdir)/$(LIB_PREFIX)intl$(LIB_SUFFIX).$(MAJOR_VERSION)
	ln -s $(LIB_PREFIX)intl$(LIB_SUFFIX).$(VERSION) $(libdir)/$(LIB_PREFIX)intl$(LIB_SUFFIX)
else
	$(INSTALL) -d $(libdir)
	$(INSTALL) $(LIBRARY) $(libdir)/$(INSTALL_LIBRARY)
endif

localuninstall:
ifneq ($(strip $(INSTALL_HEADERS)),)
	rm -f $(addprefix $(includedir)/, $(INSTALL_HEADERS))
endif
	rm -f $(libdir)/$(INSTALL_LIBRARY)

