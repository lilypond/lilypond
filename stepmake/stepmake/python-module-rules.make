
$(outdir)/%$(SHARED_MODULE_SUFFIX): $(outdir)/%.lo
	$(LD) $(SHARED_FLAGS) -o $@ $< $(ALL_LDFLAGS)

$(outdir)/%.pyc: $(outdir)/%.py
	$(PYTHON) -c 'import py_compile; py_compile.compile ("$<")'

$(outdir)/%.py: %.py
	cat $< | sed $(sed-atvariables) > $@
	chmod 755 $@
