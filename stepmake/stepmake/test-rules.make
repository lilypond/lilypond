
define MODULE_LIB_template \
$(1)/$(outdir)/library.a : \
	$(MAKE) -C $(1)
endef

$(foreach a, $(MODULE_LIBS), $(eval $(call MODULE_LIB_template,$(a))))

# yaffut.hh catches all exceptions, so re-enable -fexceptions for the tests.
$(TEST_O_FILES): EXTRA_CXXFLAGS += -fexceptions

$(TEST_EXECUTABLE): $(TEST_O_FILES) $(TEST_MODULE_LIBS:%=%/$(outdir)/library.a)
	$(call ly_progress,Making,$@,)
	$(foreach a, $(TEST_MODULE_LIBS), $(MAKE) -C $(a) && ) true
	$(CXX) -o $@ $(TEST_O_FILES) $(TEST_LOADLIBES) $(ALL_LDFLAGS)
