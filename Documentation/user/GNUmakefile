# Documentation/tex/Makefile

depth=../..


LATEX_FILES =$(wildcard *.latex)

# todo: add latex.
DVI_FILES = $(addprefix $(outdir)/, $(TELY_FILES:.tely=.dvi))

EXTRA_DIST_FILES= $(LATEX_FILES)    $(wildcard *.itexi *.itely)

HTML_FILES = $(addprefix $(outdir)/, $(TELY_FILES:.tely=.html))

PS_FILES = $(DVI_FILES:.dvi=.ps)

PS_GZ_FILES= $(addsuffix .gz, $(PS_FILES))

INFO_FILES = $(addprefix $(outdir)/, lilypond.info lilypond-internals.info)

STEPMAKE_TEMPLATES=tex texinfo documentation
LOCALSTEPMAKE_TEMPLATES=lilypond ly

# Hmm, what about?
# local-dist: check-info

info: check-info

include $(depth)/make/stepmake.make 

# Ugh,ugh.
# emacs cannot fix the menu structure when @mbinclude is used
# lilypond.tely uses mbinclude
TEXINFO_SOURCES := $(filter-out lilypond.tely, $(TEXINFO_SOURCES))

dvi: $(DVI_FILES)

ps: $(PS_FILES)


# Cancel default info generation rule.  We want to generate info from
# `.nexi', making sure we don't run LilyPond for inline pictures, when
# generating info.
$(outdir)/%.info: $(outdir)/%.texi

default: 

# Info is now built by default via texinfo-rules.
# We must build them by default, otherwise they get built during make install
info: $(INFO_FILES)


# Generic rule using % twice not possible?
# $(outdir)/%/%.html: $(outdir)/%.texi
$(outdir)/lilypond/lilypond.html: $(outdir)/lilypond.texi
	mkdir -p $(dir $@)
	$(MAKEINFO) --output=$@ --html $<
	-cp -f $(outdir)/*.png $(outdir)/lilypond
	-cp -f $(outdir)/lilypond/index.html $@
	$(deep-footify) $(shell find $(outdir)/$(*F)/*.html)

$(outdir)/lilypond-internals/lilypond-internals.html: $(outdir)/lilypond-internals.texi
	mkdir -p $(dir $@)
	$(MAKEINFO) --output=$@ --html $<
	-cp -f $(outdir)/*.png $(outdir)/lilypond-internals
	-cp -f $(outdir)/lilypond-internals/index.html $@
	$(deep-footify) $(shell find $(outdir)/$(*F)/*.html)

ifeq ($(SPLITTING_MAKEINFO),yes)

DEEP_HTML_FILES = $(outdir)/lilypond/lilypond.html $(outdir)/lilypond-internals/lilypond-internals.html

else

# Links referred to by Documentation index
LILYPOND_LINKS=Reference-Manual.html Tutorial.html Ly2dvi.html Midi2ly.html

local-WWW: deep-symlinks

deep-symlinks:
	mkdir -p $(outdir)/lilypond
	cd $(outdir)/lilypond && $(foreach i, $(LILYPOND_LINKS),\
		rm -f $(i) && ln -s lilypond.html $(i) &&) true

endif


local-WWW: $(HTML_FILES) $(datafiles) $(PS_GZ_FILES) $(DEEP_HTML_FILES) info-dir

local-WWW-clean: deep-WWW-clean

deep-WWW-clean:
	rm -rf $(outdir)/lilypond $(outdir)/lilypond-internals

info-dir:
	$(SHELL) $(buildscript-dir)/install-info-html.sh --dir=$(outdir) lilypond lilypond-internals
	$(MAKE) footify
	$(MAKE) deep-footify


$(outdir)/%.bib: %.bib
	ln -f $< $@
# we want footers even if website builds (or is built) partly
	$(MAKE) footify

local-clean:
	rm -f fonts.aux fonts.log feta*.tfm feta*.*pk 
	rm -rf $(outdir)/lilypond $(outdir)/lilypond-internals

#$(outdir)/lilypond.nexi: $(outdir)/interfaces.itexi
#$(outdir)/lilypond.texi: $(outdir)/interfaces.itexi

# Rules for the automatically generated documentation
# When cross-compiling, we don't have lilypond, so we fake
ifneq ($(CROSS),yes)


# there used to be a dependency on a dummy target, to force a rebuild of lilypond-internals every time.
# however, this triggers compilation during install, which  is a bad thing (tm).

$(outdir)/lilypond-internals.nexi $(outdir)/lilypond-internals.texi: $(depth)/$(builddir)/lily/$(outconfbase)/lilypond
	cd $(outdir) && ../$(depth)/$(builddir)/lily/$(outconfbase)/lilypond ../$(src-depth)/ly/generate-documentation
	-ln $(outdir)/lilypond-internals.texi $(outdir)/lilypond-internals.nexi


## unused
$(outdir)/interfaces.itexi: dummy
	cd $(outdir) && ../$(depth)/$(builddir)/lily/$(outconfbase)/lilypond ../$(src-depth)/ly/generate-interface-doc

else

$(outdir)/lilypond-internals.nexi $(outdir)/lilypond-internals.texi:
	touch $@
	touch $(outdir)/$(*F).nexi

$(outdir)/interfaces.itexi:
	cp dummy-interfaces.itexi $@
endif

