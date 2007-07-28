# one assumes depth and ISOLANG are defined

OUT_ITEXI_FILES = $(ITELY_FILES:%.itely=$(outdir)/%.itexi)

TEXINFO_PAPERSIZE_OPTION= $(if $(findstring $(PAPERSIZE),a4),,-t @afourpaper)

LILYPOND_BOOK_INCLUDES += \
  -I$(top-src-dir)/Documentation/user \
  -I$(top-build-dir)/Documentation/user/$(outdir)

default:

$(outdir)/lilypond.nexi: $(ITELY_FILES) $(ITEXI_FILES)

MAKEINFO = LANG=$(ISOLANG) $(MAKEINFO_PROGRAM) --force

$(outdir)/lilypond/index.html: $(outdir)/lilypond.nexi $(outdir)/user-ln doc-po
	mkdir -p $(dir $@)
	-$(MAKEINFO) -I$(outdir) --output=$(outdir)/lilypond --css-include=$(top-src-dir)/Documentation/texinfo.css --html $<
	find $(outdir) -name '*.html' | xargs grep -L 'UNTRANSLATED NODE: IGNORE ME' | xargs $(PYTHON) $(buildscript-dir)/html-gettext.py $(buildscript-dir) $(top-build-dir)/Documentation/po/$(outdir) $(ISOLANG)

# we don't make the big page until the whole manual is translated
# if this happens, we'll have to define local-WWW differently for this language
#$(outdir)/lilypond.html: $(outdir)/lilypond.nexi
#	-$(MAKEINFO) -I$(outdir) --output=$@ --css-include=$(top-src-dir)/Documentation/texinfo.css --html --no-split --no-headers $< 

$(outdir)/%.pdf: $(outdir)/%.texi $(outdir)/user-ln doc-po
	$(PYTHON) $(buildscript-dir)/texi-gettext.py $(buildscript-dir) $(top-build-dir)/Documentation/po/$(outdir) $(ISOLANG) $<
	cd $(outdir); texi2pdf --batch $(TEXINFO_PAPERSIZE_OPTION) $(notdir $*).pdftexi

local-WWW: $(outdir)/lilypond.pdf $(outdir)/lilypond/index.html
	find $(outdir) -name '*.html' | xargs grep -L --label="" 'UNTRANSLATED NODE: IGNORE ME' | sed 's!$(outdir)/!!g' | xargs $(PYTHON) $(buildscript-dir)/mass-link.py --prepend-suffix .$(ISOLANG) hard $(outdir) $(top-build-dir)/Documentation/user/$(outdir) lilypond.pdf
	find $(outdir) \( -name 'lily-??????????.png' -o -name 'lily-??????????.ly' \) -a -not -type l | sed 's!$(outdir)/!!g' | xargs $(PYTHON) $(buildscript-dir)/mass-link.py hard $(outdir) $(top-build-dir)/Documentation/user/$(outdir)

# FIXME
# ugh, this is not enough to avoid wasting build time, $(outdir)/user-ln should be touched for all languages
	touch -mr $(top-build-dir)/Documentation/user/$(outdir) $(outdir)/user-ln

LINKED_PNGS = henle-flat-gray.png baer-flat-gray.png lily-flat-bw.png

# makeinfo is broken, it MUST have PNGs in output dir
# symlinking PNGs...
# lilypond-book -I flag seems broken too, and texi2pdf -I flag confuses
# pdfetex with .aux and other files from English manual
# symlinking lily-*...
$(outdir)/user-ln: $(top-build-dir)/Documentation/user/$(outdir)
	touch -mr $(top-build-dir)/Documentation/user/$(outdir) $@
	mkdir -p $(outdir)/lilypond
	$(PYTHON) $(buildscript-dir)/mass-link.py symbolic $(top-build-dir)/Documentation/user/$(outdir) $(outdir) 'lily-*.pdf' 'lily-*.tex' 'lily-*.texi' 'lily-*.ly' 'lily-*.txt' 'lily-*.png' 'henle-flat-gray.*' 'baer-flat-gray.*' 'lily-flat-bw.*'
	cd $(outdir)/lilypond && $(foreach i, $(LINKED_PNGS), ln -sf ../../$(depth)/Documentation/user/$(i) $(i) &&) true

local-WWW-clean: deep-WWW-clean

deep-WWW-clean:
	rm -rf $(outdir)/lilypond

web-clean: clean
	$(MAKE) out=www local-WWW-clean

doc-po:
	$(MAKE) -C $(depth)/Documentation/po messages
