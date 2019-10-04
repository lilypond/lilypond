
$(O_FILES): $(outdir)/config.hh

$(SHARED_LIBRARY): $(LO_FILES)
	$(call ly_progress,Making,$@,)
	$(LD) $(SHARED_FLAGS) -o $@ $(LO_FILES) $(ALL_LDFLAGS)
