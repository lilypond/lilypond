
local-install: $(LIBRARY)
ifneq ($(strip $(INSTALL_HEADERS)),)
	$(INSTALL) -d $(DESTDIR)$(includedir)
	$(INSTALL) $(INSTALL_HEADERS) $(DESTDIR)$(includedir)
endif
ifeq ($(LIB_SUFFIX),.so)
	$(INSTALL) -d $(DESTDIR)$(libdir)
	$(INSTALL) $(LIBRARY) $(DESTDIR)$(libdir)/$(INSTALL_LIBRARY).$(VERSION)
	ln -s $(LIB_PREFIX)intl$(LIB_SUFFIX).$(VERSION) $(DESTDIR)$(libdir)/$(LIB_PREFIX)intl$(LIB_SUFFIX).$(MAJOR_VERSION)
	ln -s $(LIB_PREFIX)intl$(LIB_SUFFIX).$(VERSION) $(DESTDIR)$(libdir)/$(LIB_PREFIX)intl$(LIB_SUFFIX)
else
	$(INSTALL) -d $(DESTDIR)$(libdir)
	$(INSTALL) $(LIBRARY) $(DESTDIR)$(libdir)/$(INSTALL_LIBRARY)
endif

local-uninstall:
ifneq ($(strip $(INSTALL_HEADERS)),)
	rm -f $(addprefix $(DESTDIR)$(includedir)/, $(INSTALL_HEADERS))
endif
	rm -f $(DESTDIR)$(libdir)/$(INSTALL_LIBRARY)

