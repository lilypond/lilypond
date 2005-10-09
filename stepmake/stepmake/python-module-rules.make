
$(outdir)/%$(SHARED_MODULE_SUFFIX): $(outdir)/%.lo
	$(LD) -o $@ $< $(SHARED_FLAGS) $(ALL_LDFLAGS)


$(outdir)/%.pyc: $(outdir)/%.py
	$(PYTHON) -c 'import py_compile; py_compile.compile ("$<")'

$(outdir)/%.py: %.py $(config_make)
	cat $< | sed $(sed-atvariables) > $@
	chmod 755 $@
