# Documentation/tex/Makefile

depth=../..


LATEX_FILES =$(wildcard *.latex)

# todo: add latex.
DVI_FILES = $(addprefix $(outdir)/, $(TELY_FILES:.tely=.dvi))

EXTRA_DIST_FILES= $(LATEX_FILES) $(IMAGES)
IMAGES=$(wildcard *.png)

OUT_EPS_IMAGES=$(addprefix $(outdir)/,$(IMAGES:.png=.eps))
OUT_PNG_IMAGES=$(addprefix $(outdir)/,$(IMAGES))

HTML_FILES = $(addprefix $(outdir)/, $(TELY_FILES:.tely=.html))

PS_FILES = $(DVI_FILES:.dvi=.ps)
PDF_FILES = $(DVI_FILES:.dvi=.pdf)

PS_GZ_FILES= $(addsuffix .gz, $(PS_FILES))

INFO_DOCS = lilypond lilypond-internals music-glossary
INFO_FILES = $(INFO_DOCS:%=$(outdir)/%.info)

STEPMAKE_TEMPLATES=tex texinfo omf documentation

OMF_FILES += $(outdir)/lilypond-internals.html.omf

LOCALSTEPMAKE_TEMPLATES=lilypond ly
LILYPOND_BOOK_FLAGS=--extra-options '-e "(ly:set-option (quote internal-type-checking) \#t)"'

include $(depth)/make/stepmake.make 

# Ugh,ugh.
# emacs cannot fix the menu structure when @mbinclude is used
# lilypond.tely uses mbinclude
TEXINFO_SOURCES := $(filter-out lilypond.tely, $(TEXINFO_SOURCES))

dvi: $(DVI_FILES)

ps: $(PS_FILES)

# Cancel the default info generation rule.  We want to generate info
# from `.nexi', making sure we don't run LilyPond for inline pictures,
# when just generating info:

$(outdir)/%.info: $(outdir)/%.texi

default: 

# Info is now built by default via texinfo-rules.
# We must build them by default, otherwise they get built during make install
info: $(INFO_FILES)

local-help: extra-local-help

extra-local-help:
	@echo -e "\
  dvi         update dvi documents\n\
  info        update info pages\n\
  ps          update PostScript documents\n\
"

# Generic rule using % twice not possible?
# $(outdir)/%/%.html: $(outdir)/%.texi
$(outdir)/lilypond/lilypond.html: $(outdir)/lilypond.texi 
	mkdir -p $(dir $@)
	$(MAKEINFO) --output=$(outdir)/lilypond --html $<
	$(MAKEINFO) -I $(outdir) --output=$@ --html --no-split --no-headers $<
	-cp -f $(outdir)/*.ly $(outdir)/lilypond
	-cp -f $(outdir)/*.png $(outdir)/lilypond

$(outdir)/lilypond-internals/lilypond-internals.html: $(outdir)/lilypond-internals.texi
	mkdir -p $(dir $@)
	$(MAKEINFO) --output=$(outdir)/lilypond-internals --html $<
	$(MAKEINFO) -I $(outdir) --output=$@ --html --no-split --no-headers $<

ifeq ($(SPLITTING_MAKEINFO),yes)

$(outdir)/lilypond.dvi: $(OUT_EPS_IMAGES) $(OUT_PNG_IMAGES)

$(outdir)/%.png: %.png
	convert -resize 50x50% $< $@

$(outdir)/%.eps: %.png
	convert $< $@

DEEP_HTML_FILES = $(outdir)/lilypond/lilypond.html $(outdir)/lilypond-internals/lilypond-internals.html

else

# Links referred to by Documentation index
LILYPOND_LINKS=Reference-Manual.html Tutorial.html Ly2dvi.html Midi2ly.html

local-WWW: outimages deep-symlinks

deep-symlinks:
	mkdir -p $(outdir)/lilypond
	cd $(outdir)/lilypond && $(foreach i, $(LILYPOND_LINKS),\
		rm -f $(i) && ln -s lilypond.html $(i) &&) true

endif


local-WWW: $(HTML_FILES) $(datafiles) $(PDF_FILES) $(PS_GZ_FILES) $(DEEP_HTML_FILES) info-dir

local-WWW-clean: deep-WWW-clean

deep-WWW-clean:
	rm -rf $(outdir)/lilypond $(outdir)/lilypond-internals

info-dir:
	$(SHELL) $(buildscript-dir)/install-info-html.sh --dir=$(outdir) lilypond lilypond-internals


$(outdir)/%.bib: %.bib
	ln -f $< $@

local-clean:
	rm -f fonts.aux fonts.log feta*.tfm feta*.*pk 
	rm -rf $(outdir)/lilypond $(outdir)/lilypond-internals

#$(outdir)/lilypond.nexi: $(outdir)/interfaces.itexi
#$(outdir)/lilypond.texi: $(outdir)/interfaces.itexi

# lilypond.texi deps
$(builddir)/mf/$(outconfbase)/feta16list.ly:
	$(MAKE) -C $(topdir)/mf

# Rules for the automatically generated documentation
# When cross-compiling, we don't have lilypond, so we fake
ifneq ($(CROSS),yes)


# there used to be a dependency on a dummy target, to force a rebuild of lilypond-internals every time.
# however, this triggers compilation during install, which  is a bad thing (tm).

$(outdir)/lilypond-internals.nexi $(outdir)/lilypond-internals.texi: $(builddir)/lily/$(outconfbase)/lilypond-bin
	cd $(outdir) && $(builddir)/lily/$(outconfbase)/lilypond-bin --verbose $(abs-srcdir)/ly/generate-documentation
	-ln $(outdir)/lilypond-internals.texi $(outdir)/lilypond-internals.nexi


## unused
$(outdir)/interfaces.itexi: dummy
	cd $(outdir) && $(builddir)/lily/$(outconfbase)/lilypond-bin $(abs-srcdir)/ly/generate-interface-doc

else

$(outdir)/lilypond-internals.nexi $(outdir)/lilypond-internals.texi:
	touch $@
	touch $(outdir)/$(*F).nexi

$(outdir)/interfaces.itexi:
	cp dummy-interfaces.itexi $@
endif


local-clean: local-delete

local-delete:
	-rm -f $(outdir)/lily-1*
	-rm -f $(outdir)/*
