
$(PYTHON_MODULE): $(outdir)/config.h $(LO_FILES)
	$(LD) $(SHARED_FLAGS) -o $@ $(LO_FILES) $(LDFLAGS)
