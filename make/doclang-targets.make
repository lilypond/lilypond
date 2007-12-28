# one assumes depth and ISOLANG are defined

OUT_ITEXI_FILES = $(ITELY_FILES:%.itely=$(outdir)/%.itexi)

TEXINFO_PAPERSIZE_OPTION= $(if $(findstring $(PAPERSIZE),a4),,-t @afourpaper)

LILYPOND_BOOK_INCLUDES += \
  -I$(top-src-dir)/Documentation/user \
  -I$(top-build-dir)/Documentation/user/$(outdir)

default:

$(outdir)/%.nexi: $(ITELY_FILES) $(ITEXI_FILES)

MAKEINFO_FLAGS += --force

$(outdir)/%/index.html: $(outdir)/%.nexi $(outdir)/user-ln doc-po
	mkdir -p $(dir $@)
	-$(MAKEINFO) $(MAKEINFO_FLAGS) -I$(outdir) --output=$(outdir)/$* --css-include=$(top-src-dir)/Documentation/texinfo.css --html $<

# we don't make the big page until the whole manual is translated
# if this happens, we'll have to define local-WWW differently for this language
#$(outdir)/lilypond.html: $(outdir)/lilypond.nexi
#	-$(MAKEINFO) $(MAKEINFO_FLAGS) -I$(outdir) --output=$@ --css-include=$(top-src-dir)/Documentation/texinfo.css --html --no-split --no-headers $< 

$(outdir)/%.pdf: $(outdir)/%.texi $(outdir)/user-ln doc-po
	$(PYTHON) $(buildscript-dir)/texi-gettext.py $(buildscript-dir) $(top-build-dir)/Documentation/po/$(outdir) $(ISOLANG) $<
	cd $(outdir); texi2pdf --batch $(TEXINFO_PAPERSIZE_OPTION) $(notdir $*).pdftexi

TELY_FILES = $(call src-wildcard,*.tely)
DEEP_HTML_FILES = $(TELY_FILES:%.tely=$(outdir)/%/index.html)
PDF_FILES = $(TELY_FILES:%.tely=$(outdir)/%.pdf)

local-WWW: $(DEEP_HTML_FILES) $(PDF_FILES)
	find $(outdir) -name '*.html' | xargs grep -L 'UNTRANSLATED NODE: IGNORE ME' | xargs $(PYTHON) $(buildscript-dir)/html-gettext.py $(buildscript-dir) $(top-build-dir)/Documentation/po/$(outdir) $(ISOLANG)
	find $(outdir) -name '*.html' | xargs grep -L --label="" 'UNTRANSLATED NODE: IGNORE ME' | sed 's!$(outdir)/!!g' | xargs $(PYTHON) $(buildscript-dir)/mass-link.py --prepend-suffix .$(ISOLANG) hard $(outdir) $(top-build-dir)/Documentation/user/$(outdir) $(TELY_FILES:%.tely=%.pdf)
	find $(outdir) \( -name 'lily-??????????.png' -o -name 'lily-??????????.ly' \) -a -not -type l | sed 's!$(outdir)/!!g' | xargs $(PYTHON) $(buildscript-dir)/mass-link.py hard $(outdir) $(top-build-dir)/Documentation/user/$(outdir)

LINKED_PNGS = henle-flat-gray.png baer-flat-gray.png lily-flat-bw.png

# makeinfo MUST have PNGs in cwd for info images to work
# symlinking PNGs...
# texi2pdf -I flag confuses pdfetex with .aux and other files
# from English manual
# symlinking lily-*...
$(outdir)/user-ln: $(top-build-dir)/Documentation/user/$(outdir)
	$(PYTHON) $(buildscript-dir)/mass-link.py symbolic $(top-build-dir)/Documentation/user/$(outdir) $(outdir) 'lily-*.pdf' 'lily-*.tex' 'lily-*.texi' 'lily-*.ly' 'lily-*.txt' 'lily-*.png' 'henle-flat-gray.*' 'baer-flat-gray.*' 'lily-flat-bw.*' 'context-example.*'
	mkdir -p $(outdir)/lilypond
	cd $(outdir)/lilypond && $(foreach i, $(LINKED_PNGS), ln -sf ../../$(depth)/Documentation/user/$(i) $(i) &&) true
	touch -mr $(top-build-dir)/Documentation/user/$(outdir) $@

local-WWW-clean: deep-WWW-clean

deep-WWW-clean:
	rm -rf $(outdir)/lilypond*

web-clean: clean
	$(MAKE) out=www local-WWW-clean

doc-po:
	$(MAKE) -C $(depth)/Documentation/po messages
