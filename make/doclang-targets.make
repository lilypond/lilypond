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
	find $(outdir) -name '*.html' | xargs grep -L 'UNTRANSLATED NODE: IGNORE ME' | xargs $(PYTHON) $(buildscript-dir)/html-gettext.py $(buildscript-dir) $(top-src-dir)/Documentation/po/$(outdir) $(ISOLANG)

$(outdir)/lilypond.html: $(outdir)/lilypond.nexi
	-$(MAKEINFO) -I$(outdir) --output=$@ --css-include=$(top-src-dir)/Documentation/texinfo.css --html --no-split --no-headers $< 

local-WWW: png-ln $(outdir)/lilypond.html $(outdir)/lilypond/index.html deep-ln lang-merge

lang-merge:
	$(foreach i, $(shell find $(outdir) -name '*.html' | xargs grep -L --label="" 'UNTRANSLATED NODE: IGNORE ME'), ln -f $(i) $(i:$(outdir)/%.html=$(depth)/Documentation/user/$(outdir)/%.$(ISOLANG).html) &&) true

png-ln:
	mkdir -p $(outdir)/lilypond
	# makeinfo is broken, it MUST have PNGs in output dir
	# symlinking PNGs...
	$(foreach i, $(shell find $(depth)/Documentation/user/$(outdir) -maxdepth 1 -name '*.png'), ln -sf ../$(i) $(i:$(depth)/Documentation/user/$(outdir)/%.png=$(outdir)/%.png) &&) true
	$(foreach i, $(shell find $(depth)/Documentation/user/$(outdir)/lilypond -name '*.png'), ln -sf ../../$(i) $(i:$(depth)/Documentation/user/$(outdir)/%.png=$(outdir)/%.png) &&) true

# Links referred to by Documentation index
# BROKEN: the following makes broken symlinks
#LILYPOND_LINKS = Reference-Manual.html Tutorial.html Ly2dvi.html Midi2ly.html


deep-ln:
	mkdir -p $(outdir)/lilypond
	cd $(outdir)/lilypond && $(foreach i, $(LILYPOND_LINKS),\
		 rm -f $(i) && ln -s lilypond.html $(i) &&) true

local-WWW-clean: deep-WWW-clean

deep-WWW-clean:
	rm -rf $(outdir)/lilypond

web-clean: clean
	$(MAKE) out=www local-WWW-clean

doc-po:
	$(MAKE) -C $(depth)/Documentation/po messages
