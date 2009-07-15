default:

ifeq ($(out),www)
ifeq ($(ISOLANG),ja)
local-WWW-1: $(MASTER_TEXI_FILES) $(XREF_MAPS_FILES)
endif
ifneq ($(ISOLANG),ja)
local-WWW-1: $(MASTER_TEXI_FILES) $(PDF_FILES) $(XREF_MAPS_FILES)
endif

local-WWW-2: $(DEEP_HTML_FILES) $(BIG_PAGE_HTML_FILES) $(DOCUMENTATION_LOCALE_TARGET)

$(DOCUMENTATION_LOCALE_TARGET):
	$(MAKE) -C $(depth)/Documentation/po out=www messages
	touch $@
endif
