depth=../..

LATEX_FILES =$(wildcard *.latex)

# todo: add latex.
DVI_FILES = $(addprefix $(outdir)/, $(TELY_FILES:.tely=.dvi))

EXTRA_DIST_FILES= $(LATEX_FILES) $(IMAGES)
IMAGES=$(wildcard *.png)

OUT_EPS_IMAGES=$(addprefix $(outdir)/,$(IMAGES:.png=.eps))
OUT_PNG_IMAGES=$(addprefix $(outdir)/,$(IMAGES))
OUT_ITEXI_FILES=$(addprefix $(outdir)/,$(ITELY_FILES:.itely=.itexi) 
HTML_FILES = $(addprefix $(outdir)/, $(TELY_FILES:.tely=.html))

PS_FILES = $(DVI_FILES:.dvi=.ps)
PDF_FILES = $(DVI_FILES:.dvi=.pdf)

PS_GZ_FILES= $(addsuffix .gz, $(PS_FILES))

INFO_DOCS = lilypond lilypond-internals music-glossary
INFO_FILES = $(INFO_DOCS:%=$(outdir)/%.info)

STEPMAKE_TEMPLATES=tex texinfo omf documentation

OMF_FILES += $(outdir)/lilypond-internals.html.omf

LOCALSTEPMAKE_TEMPLATES=lilypond ly
LILYPOND_BOOK_FLAGS = --process="lilypond-bin -I $(srcdir)/input/test -e '(ly:set-option (quote internal-type-checking) \#t)'"

include $(depth)/make/stepmake.make 

dvi: $(DVI_FILES)

ps: $(PS_FILES)

info: $(INFO_FILES)

# There are two modes for info: with and without images.
ifeq ($(out),www)

# This builds all .info targets with images, in out-www.
# Viewawble with a recent Emacs, doing: M-x info out-www/lilypond.info

#info: $(INFO_FILES)
    # Cancel the special, non-image info generation rule that skips images:
    $(outdir)/%.info: $(outdir)/%.nexi
local-install-info: install-info info
	-$(INSTALL) -d $(DESTDIR)$(package_infodir)
ifneq ($(patsubst %/local,%,$(DESTDIR)$(prefix)),/usr)
## Can not have absolute symlinks because some binary packages build schemes
## install files in nonstandard root.  Best we can do is to notify the
## builder or packager.
	@echo "***"
	@echo "For images in the INFO docs to work, do: "
	@echo "    (cd $(package_infodir) && ln -sf ../../doc/lilypond/$(TOPLEVEL_VERSION)/Documentation/user/out-www/*png .)"
	@echo "or add something like that to the postinstall script."
	@echo "***"
else
	@echo "    (cd $(package_infodir) && ln -sf $(local_package_docdir)/Documentation/user/out-www/*png .)"
endif

local-uninstall-WWW:
	rm -f $(package_infodir)/*.png
else
    # Cancel the default info generation rule that generates images:
    $(outdir)/%.info: # $(outdir)/%.texi
endif

# All web targets, except info image symlinks and info docs are
# installed in non-recursing target from TOPDIR
local-install-WWW: local-install-info
local-uninstall-WWW: local-uninstall-info

default: 


local-help: extra-local-help

extra-local-help:
	@echo -e "\
  dvi         update dvi documents\n\
  info        update info pages\n\
  ps          update PostScript documents\n\
"

# Generic rule using % twice not possible?
# $(outdir)/%/%.html: $(outdir)/%.texi
$(outdir)/lilypond.texi: $(outdir)/lilypond-internals.texi
$(outdir)/lilypond.nexi: $(outdir)/lilypond-internals.texi


$(outdir)/lilypond/lilypond.html: $(outdir)/lilypond.texi 
	mkdir -p $(dir $@)
	$(MAKEINFO) -I$(outdir) --output=$(outdir)/lilypond --html $<
	$(MAKEINFO) -I$(outdir) --output=$@ --html --no-split --no-headers $<
	perl -i~ -pe 's!../lilypond-internals!lilypond-internals/!g' $(outdir)/lilypond.html
	rm -f $(outdir)/lilypond/*.png $(outdir)/lilypond/*.ly 
	-ln -f $(outdir)/*.png $(outdir)/*.ly $(outdir)/lilypond/

$(outdir)/lilypond-internals/lilypond-internals.html: $(outdir)/lilypond-internals.texi
	mkdir -p $(dir $@)
	$(MAKEINFO) --output=$(outdir)/lilypond-internals --html $<
	$(MAKEINFO) -I$(outdir) --output=$@ --html --no-split --no-headers $<

ifeq ($(SPLITTING_MAKEINFO),yes)

$(outdir)/lilypond.dvi: $(OUT_EPS_IMAGES) $(OUT_PNG_IMAGES)

$(outdir)/%.png: %.png
	convert -geometry 50x50% $< $@

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

# Symlinks to refer to external source documents from split and non-split HTML
source-links = $(outdir)/source $(outdir)/lilypond/source # $(outdir)/music-glossary/source

$(outdir)/source:
	@rm -f $(@)
	ln -sf ../../../ $(@)

$(outdir)/lilypond/source:
	@rm -f $(@)
	ln -sf ../../../../ $(@)

# not available in 2.2 or 2.4 as split document
#$(outdir)/music-glossary/source:
#	@rm -f $(@)
#	ln -sf ../../../../ $(@)

local-WWW: $(HTML_FILES) $(datafiles) $(PDF_FILES) $(PS_GZ_FILES) $(DEEP_HTML_FILES) $(source-links) info info-dir

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

# lilypond.texi deps
$(builddir)/mf/$(outconfbase)/feta16list.ly:
	$(MAKE) -C $(topdir)/mf

$(outdir)/lilypond.texi: $(ITELY_FILES) macros.itexi 
$(outdir)/lilypond.nexi: $(ITELY_FILES) macros.itexi 

# Rules for the automatically generated documentation
# When cross-compiling, we don't have lilypond, so we fake
ifneq ($(CROSS),yes)


# There used to be a dependency on a dummy target, to force a rebuild
# of lilypond-internals every time.  however, this triggers
# compilation during install, which is a bad thing (tm).

$(outdir)/lilypond-internals.nexi $(outdir)/lilypond-internals.texi: $(builddir)/lily/$(outconfbase)/lilypond-bin
	cd $(outdir) && $(builddir)/lily/$(outconfbase)/lilypond-bin --verbose $(abs-srcdir)/ly/generate-documentation
	rm -f $(outdir)/lilypond-internals.nexi
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
