default:

# BIG_PAGE_HTML_FILES is defined differently in each language makefile
local-WWW: $(DEEP_HTML_FILES) $(PDF_FILES) doc-po $(BIG_PAGE_HTML_FILES)
	find $(outdir) -name '*.html' | xargs grep -L 'UNTRANSLATED NODE: IGNORE ME' | xargs $(PYTHON) $(buildscript-dir)/html-gettext.py $(buildscript-dir) $(top-build-dir)/Documentation/po/$(outdir) $(ISOLANG)
	find $(outdir) -name '*.html' | xargs grep -L --label="" 'UNTRANSLATED NODE: IGNORE ME' | sed 's!$(outdir)/!!g' | xargs $(PYTHON) $(buildscript-dir)/mass-link.py --prepend-suffix .$(ISOLANG) hard $(outdir) $(top-build-dir)/Documentation/user/$(outdir) $(TELY_FILES:%.tely=%.pdf)
	find $(outdir) \( -name 'lily-??????????.png' -o -name 'lily-??????????.ly' \) | sed 's!$(outdir)/!!g' | xargs $(PYTHON) $(buildscript-dir)/mass-link.py hard $(outdir) $(top-build-dir)/Documentation/user/$(outdir)


local-WWW-clean: deep-WWW-clean

deep-WWW-clean:
	rm -rf $(outdir)/lilypond*

web-clean: clean
	$(MAKE) out=www local-WWW-clean

doc-po:
	$(MAKE) -C $(depth)/Documentation/po out=www messages
