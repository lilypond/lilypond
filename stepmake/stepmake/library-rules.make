
$(LIBRARY): $(outdir)/config.h $(O_FILES)
	$(AR) $(ARFLAGS) $@ $(O_FILES)
	# thanks to Nelson Beebe for this trick.
	$(RANLIB) $@ || $(AR) ts $@ || true



