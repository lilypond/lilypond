
$(outdir)/%$(SHARED_LIB_SUFFIX): $(outdir)/%.lo
	$(LD) $(SHARED_FLAGS) -o $@ $< $(LDFLAGS)
