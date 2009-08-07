default:

ifeq ($(out),www)
ifeq ($(ISOLANG),ja)
local-WWW-1: $(MASTER_TEXI_FILES) $(XREF_MAPS_FILES)
endif
ifneq ($(ISOLANG),ja)
local-WWW-1: $(MASTER_TEXI_FILES) $(PDF_FILES) $(XREF_MAPS_FILES)
endif

local-WWW-2: $(DEEP_HTML_FILES) $(BIG_PAGE_HTML_FILES) $(DOCUMENTATION_LOCALE_TARGET) $(OUT_HTML_FILES)
# FIXME: move the following line to a rule that generate the right file
	$(buildscript-dir)/mass-link --prepend-suffix .$(ISOLANG) hard $(outdir) $(top-build-dir)/Documentation/$(outdir) $(HTML_FILES)

$(DOCUMENTATION_LOCALE_TARGET):
	$(MAKE) -C $(depth)/Documentation/po out=www messages
	touch $@
endif
