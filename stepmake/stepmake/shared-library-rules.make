
$(SHARED_LIBRARY): $(outdir)/config.h $(LO_FILES)
	$(LD) $(SHARED_FLAGS) -o $@.$(VERSION) $(LO_FILES) $(LDFLAGS)
	rm -f $@.$(MAJOR_VERSION)
	ln -sf $(outdir)/$(LIB_PREFIX)$(NAME).so.$(VERSION) $@.$(MAJOR_VERSION)
	rm -f $@
	ln -sf $(LIB_PREFIX)$(NAME).so.$(VERSION) $@

