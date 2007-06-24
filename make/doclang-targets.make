# one assumes depth and ISOLANG are defined

OUT_ITEXI_FILES = $(ITELY_FILES:%.itely=$(outdir)/%.itexi)

TEXINFO_PAPERSIZE_OPTION= $(if $(findstring $(PAPERSIZE),a4),,-t @afourpaper)

LILYPOND_BOOK_INCLUDES += \
  -I$(top-src-dir)/Documentation/user \
  -I$(top-build-dir)/Documentation/user/$(outdir)

default:

$(outdir)/lilypond.nexi: $(ITELY_FILES) $(ITEXI_FILES)

MAKEINFO = LANG=$(ISOLANG) $(MAKEINFO_PROGRAM) --force

$(outdir)/lilypond/index.html: $(outdir)/lilypond.nexi user-ln doc-po
	mkdir -p $(dir $@)
	-$(MAKEINFO) -I$(outdir) --output=$(outdir)/lilypond --css-include=$(top-src-dir)/Documentation/texinfo.css --html $<
	find $(outdir) -name '*.html' | xargs grep -L 'UNTRANSLATED NODE: IGNORE ME' | xargs $(PYTHON) $(buildscript-dir)/html-gettext.py $(buildscript-dir) $(top-build-dir)/Documentation/po/$(outdir) $(ISOLANG)

# we don't make the big page until the whole manual is translated
# if this happens, we'll have to define local-WWW differently for this language
#$(outdir)/lilypond.html: $(outdir)/lilypond.nexi
#	-$(MAKEINFO) -I$(outdir) --output=$@ --css-include=$(top-src-dir)/Documentation/texinfo.css --html --no-split --no-headers $< 

$(outdir)/%.pdf: $(outdir)/%.texi user-ln doc-po
	$(PYTHON) $(buildscript-dir)/texi-gettext.py $(buildscript-dir) $(top-build-dir)/Documentation/po/$(outdir) $(ISOLANG) $<
	cp $< $(<)~
	cd $(outdir); texi2pdf --batch $(TEXINFO_PAPERSIZE_OPTION) $(<F)

local-WWW: $(outdir)/lilypond.pdf $(outdir)/lilypond/index.html
	$(foreach i, $(shell find $(outdir) -name '*.html' | xargs grep -L --label="" 'UNTRANSLATED NODE: IGNORE ME'), ln -f $(i) $(i:$(outdir)/%.html=$(depth)/Documentation/user/$(outdir)/%.$(ISOLANG).html) &&) ln -f $(outdir)/lilypond.pdf $(depth)/Documentation/user/$(outdir)/lilypond.$(ISOLANG).pdf && true

LINKED_PNGS = henle-flat-gray.png baer-flat-gray.png lily-flat-bw.png

# makeinfo is broken, it MUST have PNGs in output dir
# symlinking PNGs...
# lilypond-book -I flag seems broken too, and texi2pdf -I flag confuses
# pdfetex with .aux and other files from English manual
# symlinking lily-*...
user-ln:
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
