default:

local-WWW-1: $(MASTER_TEXI_FILES) $(PDF_FILES) $(XREF_MAPS_FILES)

# BIG_PAGE_HTML_FILES is defined differently in each language makefile
local-WWW-2: $(DEEP_HTML_FILES) $(BIG_PAGE_HTML_FILES) $(DOCUMENTATION_LOCALE_TARGET)
	find $(outdir) -name '*.html' | xargs grep -L 'UNTRANSLATED NODE: IGNORE ME' | xargs $(PYTHON) $(buildscript-dir)/html-gettext.py $(ISOLANG)
	find $(outdir) -name '*.html' | xargs grep -L --label="" 'UNTRANSLATED NODE: IGNORE ME' | sed 's!$(outdir)/!!g' | xargs $(PYTHON) $(buildscript-dir)/mass-link.py --prepend-suffix .$(ISOLANG) hard $(outdir) $(top-build-dir)/Documentation/user/$(outdir) $(TELY_FILES:%.tely=%.pdf)
	find $(outdir) \( -name 'lily-*.png' -o -name 'lily-*.ly' \) | sed 's!$(outdir)/!!g' | xargs $(PYTHON) $(buildscript-dir)/mass-link.py hard $(outdir) $(top-build-dir)/Documentation/user/$(outdir)

$(DOCUMENTATION_LOCALE_TARGET):
	$(MAKE) -C $(depth)/Documentation/po out=www messages
	touch $@
