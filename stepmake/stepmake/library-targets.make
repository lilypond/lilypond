
default: $(LIBRARY)

$(outdir)/library.a: $(outdir)/config.h $(O_FILES) $(MODULE_LIBES)
	$(AR_COMMAND) $(O_FILES)
	$(RANLIB_COMMAND)

$(SHAREDLIBRARY):  $(outdir)/config.h $(O_FILES) $(MODULE_LIBES)
	$(LD_COMMAND) $(O_FILES) -o $@.$(VERSION)
	rm -f $@
	ln -sf $(outdir)/$(LIB_PREFIX)$(NAME).so.$(VERSION) $@.$(MAJOR_VERSION)
	ln -sf $(LIB_PREFIX)$(NAME).so.$(VERSION) $@


