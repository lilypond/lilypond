

$(SHARED_LIBRARY): $(outdir)/config.hh $(LO_FILES)
	$(LD) $(SHARED_FLAGS) -o $@ $(LO_FILES) $(ALL_LDFLAGS)
