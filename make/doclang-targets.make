# assumes depth and ISOLANG are defined

OUT_ITEXI_FILES = $(ITELY_FILES:%.itely=$(outdir)/%.itexi)

LILYPOND_BOOK_INCLUDES += \
  -I$(top-src-dir)/Documentation/user \
  -I$(top-build-dir)/Documentation/user/$(outdir)

default:

$(outdir)/lilypond.nexi: $(ITELY_FILES) $(ITEXI_FILES)

MAKEINFO = LANG=$(ISOLANG) $(MAKEINFO_PROGRAM) --force

$(outdir)/lilypond/index.html: $(outdir)/lilypond.nexi doc-po
	mkdir -p $(dir $@)
	-$(MAKEINFO) -I$(outdir) --output=$(outdir)/lilypond --css-include=$(top-src-dir)/Documentation/texinfo.css --html $<
	find $(outdir) -name '*.html' | xargs grep -L 'UNTRANSLATED NODE: IGNORE ME' | xargs $(PYTHON) $(buildscript-dir)/html-gettext.py $(buildscript-dir) $(top-build-dir)/Documentation/po/$(outdir) $(ISOLANG)

# we don't make the big page until the whole manual is translated
# if this happens, we'll have to define local-WWW differently for this language
#$(outdir)/lilypond.html: $(outdir)/lilypond.nexi
#	-$(MAKEINFO) -I$(outdir) --output=$@ --css-include=$(top-src-dir)/Documentation/texinfo.css --html --no-split --no-headers $< 

local-WWW: png-ln $(outdir)/lilypond/index.html lang-merge

lang-merge:
	$(foreach i, $(shell find $(outdir) -name '*.html' | xargs grep -L --label="" 'UNTRANSLATED NODE: IGNORE ME'), ln -f $(i) $(i:$(outdir)/%.html=$(depth)/Documentation/user/$(outdir)/%.$(ISOLANG).html) &&) true

LINKED_PNGS = henle-flat-gray.png baer-flat-gray.png lily-flat-bw.png

# makeinfo MUST have PNGs in cwd for info images to work
png-ln:
	mkdir -p $(outdir)/lilypond
	cd $(outdir) && $(foreach i, $(LINKED_PNGS), ln -sf ../$(depth)/Documentation/user/$(i) $(i) &&) true
	cd $(outdir)/lilypond && $(foreach i, $(LINKED_PNGS), ln -sf ../../$(depth)/Documentation/user/$(i) $(i) &&) true

local-WWW-clean: deep-WWW-clean

deep-WWW-clean:
	rm -rf $(outdir)/lilypond

web-clean: clean
	$(MAKE) out=www local-WWW-clean

doc-po:
	$(MAKE) -C $(depth)/Documentation/po messages
