
$(outdir)/%$(SHARED_MODULE_SUFFIX): $(outdir)/%.lo
	$(LD) $(SHARED_FLAGS) -o $@ $< $(ALL_LDFLAGS)
