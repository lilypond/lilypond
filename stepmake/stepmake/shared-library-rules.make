

$(SHARED_LIBRARY): $(outdir)/config.h $(LO_FILES)
	$(LD) $(SHARED_FLAGS) -o $@ $(LO_FILES) $(ALL_LDFLAGS)
