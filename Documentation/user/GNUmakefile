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

include $(depth)/make/stepmake.make 

dvi: $(DVI_FILES)

ps: $(PS_FILES)

info: $(INFO_FILES)

xml: $(outdir)/lilypond/lilypond.xml $(outdir)/lilypond-internals/lilypond-internals.xml

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
	@echo "    (cd $(package_infodir) && ln -sf ../../doc/lilypond/Documentation/user/out-www/*png .)"
	@echo "or add something like that to the postinstall script."
	@echo "***"
else
	(cd $(package_infodir) && ln -sf $(local_package_docdir)/Documentation/user/out-www/*png .)
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
  xml	      update Docbook xml documentation\n\
"

# Generic rule using % twice not possible?
# $(outdir)/%/%.html: $(outdir)/%.texi
$(outdir)/lilypond.texi: $(outdir)/lilypond-internals.texi
$(outdir)/lilypond.nexi: $(outdir)/lilypond-internals.texi


$(outdir)/lilypond/lilypond.html: $(outdir)/lilypond.texi 
	mkdir -p $(dir $@)
	$(MAKEINFO) -I$(outdir) --output=$(outdir)/lilypond --css-include=$(abs-srcdir)/Documentation/texinfo.css --html $<
	$(MAKEINFO) -I$(outdir) --output=$@ --css-include=$(abs-srcdir)/Documentation/texinfo.css --html --no-split --no-headers $<
	perl -i~ -pe 's!../lilypond-internals!lilypond-internals/!g' $(outdir)/lilypond.html
	find $(outdir)/lilypond/ -name '*'.png -o -name '*'.ly | xargs rm -f
# symbolic links to save space 
	(cd $(outdir)/lilypond/ ; ln -sf ../*.png ../*.ly . )

$(outdir)/lilypond-internals/lilypond-internals.html: $(outdir)/lilypond-internals.texi
	mkdir -p $(dir $@)
	$(MAKEINFO) --output=$(outdir)/lilypond-internals --css-include=$(abs-srcdir)/Documentation/texinfo.css --html $<
	$(MAKEINFO) -I$(outdir) --output=$@ --css-include=$(abs-srcdir)/Documentation/texinfo.css --html --no-split --no-headers $<

$(outdir)/lilypond/lilypond.xml: $(outdir)/lilypond.texi 
	mkdir -p $(dir $@)
	$(MAKEINFO) -I$(outdir) --output=$@ --docbook $<
	perl -i~ -pe 's!../lilypond-internals!lilypond-internals/!g' $(outdir)/lilypond.xml

$(outdir)/lilypond-internals/lilypond-internals.xml: $(outdir)/lilypond-internals.texi
	mkdir -p $(dir $@)
	$(MAKEINFO) --output=$(outdir)/lilypond-internals --docbook $<

$(outdir)/lilypond.dvi: $(OUT_EPS_IMAGES) $(OUT_PNG_IMAGES)

$(outdir)/%.png: %.png
	convert -geometry 50x50% $< $@

$(outdir)/%.eps: %.png
	convert $< $@

DEEP_HTML_FILES = $(outdir)/lilypond/lilypond.html $(outdir)/lilypond-internals/lilypond-internals.html

local-WWW: $(HTML_FILES) $(datafiles) $(PDF_FILES) $(PS_GZ_FILES) $(DEEP_HTML_FILES) info info-dir

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

$(outdir)/lilypond.texi: $(ITELY_FILES) $(ITEXI_FILES) 
$(outdir)/lilypond.nexi: $(ITELY_FILES) $(ITEXI_FILES)

# Rules for the automatically generated documentation
# When cross-compiling, we don't have lilypond, so we fake
ifneq ($(CROSS),yes)


# There used to be a dependency on a dummy target, to force a rebuild
# of lilypond-internals every time.  however, this triggers
# compilation during install, which is a bad thing (tm).

$(outdir)/lilypond-internals.nexi $(outdir)/lilypond-internals.texi: $(builddir)/lily/$(outconfbase)/lilypond
	cd $(outdir) && $(builddir)/lily/$(outconfbase)/lilypond --verbose $(abs-srcdir)/ly/generate-documentation
	rm -f $(outdir)/lilypond-internals.nexi
	-ln $(outdir)/lilypond-internals.texi $(outdir)/lilypond-internals.nexi


## unused
$(outdir)/interfaces.itexi: dummy
	cd $(outdir) && $(builddir)/lily/$(outconfbase)/lilypond $(abs-srcdir)/ly/generate-interface-doc

else

$(outdir)/lilypond-internals.nexi $(outdir)/lilypond-internals.texi:
	touch $@
	touch $(outdir)/$(*F).nexi

$(outdir)/interfaces.itexi:
	cp dummy-interfaces.itexi $@
endif


local-clean: local-delete

local-delete:
	-for i in $$(seq 0 9); do \
		for j in dvi eps eps.bbox eps.trans log png tex; do \
			rm -f $(outdir)/lily-$$i*$$j; \
		done; \
	done
	-rm -f $(outdir)/*
