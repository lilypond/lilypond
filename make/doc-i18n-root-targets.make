default:

ifeq ($(out),www)
ifneq ($(NO_PDF_FILES),)
local-WWW-1: $(OUT_TEXINFO_MANUALS) $(MASTER_TEXI_FILES) $(XREF_MAPS_FILES)
endif
ifeq ($(NO_PDF_FILES),)
local-WWW-1: $(OUT_TEXINFO_MANUALS) $(MASTER_TEXI_FILES) $(PDF_FILES) $(XREF_MAPS_FILES)
endif

local-WWW-2: $(DEEP_HTML_FILES) $(BIG_PAGE_HTML_FILES) $(DOCUMENTATION_LOCALE_TARGET)
# FIXME: move the following line to a rule that generate the right file
	(echo | grep -L 'UNTRANSLATED NODE: IGNORE ME' $(HTML_FILES)) | xargs $(buildscript-dir)/mass-link --prepend-suffix .$(ISOLANG) hard $(outdir) $(top-build-dir)/Documentation/$(outdir)

$(DOCUMENTATION_LOCALE_TARGET):
	$(MAKE) -C $(depth)/Documentation/po out=www messages
	touch $@
endif
