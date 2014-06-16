
define MODULE_LIB_template \
$(1)/$(outdir)/library.a : \
	$(MAKE) -C $(1)
endef

$(foreach a, $(MODULE_LIBS), $(eval $(call MODULE_LIB_template,$(a))))

$(O_FILES): $(outdir)/config.hh

$(EXECUTABLE): $(O_FILES) $(outdir)/version.hh $(MODULE_LIBS:%=%/$(outdir)/library.a)
	$(foreach a, $(MODULE_LIBS), $(MAKE) -C $(a) && ) true
	$(LD) -o $@ $(O_FILES) $(LOADLIBES) $(ALL_LDFLAGS)
