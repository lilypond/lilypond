
$(EXECUTABLE): $(outdir)/config.h $(O_FILES) $(outdir)/version.hh
	$(foreach a, $(MODULE_LIBS), $(MAKE) -C $(a) && ) true
	$(LD) -o $@ $(O_FILES) $(LOADLIBES) $(ALL_LDFLAGS)
