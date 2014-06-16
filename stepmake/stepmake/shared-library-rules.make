
$(O_FILES): $(outdir)/config.hh

$(SHARED_LIBRARY): $(LO_FILES)
	$(LD) $(SHARED_FLAGS) -o $@ $(LO_FILES) $(ALL_LDFLAGS)
