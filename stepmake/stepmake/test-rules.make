
define MODULE_LIB_template \
$(1)/$(outdir)/library.a : \
	$(MAKE) -C $(1)
endef

$(foreach a, $(MODULE_LIBS), $(eval $(call MODULE_LIB_template,$(a))))

$(TEST_EXECUTABLE): $(outdir)/config.hh $(TEST_O_FILES) $(TEST_MODULE_LIBS:%=%/$(outdir)/library.a)
	$(foreach a, $(TEST_MODULE_LIBS), $(MAKE) -C $(a) && ) true
	$(LD) -o $@ $(TEST_O_FILES) $(TEST_LOADLIBES) $(ALL_LDFLAGS)
