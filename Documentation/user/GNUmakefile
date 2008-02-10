depth=../..

LATEX_FILES =$(call src-wildcard,*.latex)


EXTRA_DIST_FILES = $(LATEX_FILES) $(IMAGES) $(EPS_ILLUSTRATIONS)
EXTRA_DIST_FILES += README.txt writing-texinfo.txt policy.txt

IMAGES=$(call src-wildcard,*.png)
EPS_ILLUSTRATIONS=context-example.eps
PDF_ILLUSTRATIONS=context-example.pdf

OUT_PDF_IMAGES=$(IMAGES:%.png=$(outdir)/%.pdf) $(addprefix $(outdir)/,$(PDF_ILLUSTRATIONS))

OUT_PNG_IMAGES=$(OUT_PDF_IMAGES:%.pdf=%.png)

OUT_TEXI_FILES=$(ITEXI_FILES:%.itexi=$(outdir)/%.texi)\
 $(ITELY_FILES:%.itely=$(outdir)/%.texi)
HTML_FILES = $(TELY_FILES:%.tely=$(outdir)/%-big-page.html)\
 $(outdir)/lilypond-internals-big-page.html

# todo: add latex.
PDF_FILES = $(TELY_FILES:%.tely=$(outdir)/%.pdf)
# this prevents aux files from being reused for translated docs
TEXI2PDF_FLAGS = --tidy

INFO_DOCS = lilypond lilypond-internals music-glossary lilypond-program lilypond-learning
INFO_FILES = $(INFO_DOCS:%=$(outdir)/%.info)

ifeq ($(out),www)
INFO_IMAGES_DIR = lilypond
endif

STEPMAKE_TEMPLATES=tex texinfo omf documentation
OMF_FILES += $(outdir)/lilypond-internals.html.omf

LOCALSTEPMAKE_TEMPLATES=lilypond ly


TEXINPUTS=$(top-src-dir)/tex::
export TEXINPUTS

include $(depth)/make/stepmake.make

info: $(INFO_FILES)

	@echo export LILYPOND_DATADIR=$(LILYPOND_DATADIR)
	@echo export PYTHONPATH=$(PYTHONPATH)

xml: $(outdir)/lilypond/lilypond.xml $(outdir)/lilypond-internals/lilypond-internals.xml

# There are two modes for info: with and without images.
ifeq ($(out),www)

# This builds all .info targets with images, in out-www.
# Viewable with a recent Emacs, doing: C-u C-h i out-www/lilypond.info

# Cancel the special, non-image info generation rule that skips images:
$(outdir)/%.info: $(outdir)/%.nexi

# "makeinfo --info" MUST be able to read PNGs from CWD for info images
# to work, hence lilypond/ -> $(outdir)/ symlink.
# $(outdir)/lilypond/*.png symlinks are only needed to view
# out-www/*.info with Emacs -- HTML docs no longer need these
# symlinks, see replace_symlinks_urls in
# buildscripts/add_html_footer.py.
$(outdir)/%.info: lilypond
lilypond: $(OUT_PNG_IMAGES)
	rm -f lilypond
	ln -s $(outdir) lilypond
	mkdir -p $(outdir)/lilypond
	find $(outdir)/lilypond/ -name '*'.png | xargs rm -f
	(cd $(outdir)/lilypond/ ; ln -sf ../*.png . )

local-install-info: info
	-$(INSTALL) -d $(DESTDIR)$(infodir)
ifneq ($(patsubst %/local,%,$(DESTDIR)$(prefix)),/usr)
## Can not have absolute symlinks because some binary packages build schemes
## install files in nonstandard root.  Best we can do is to notify the
## builder or packager.
	@echo
	@echo "***************************************************************"
	@echo "Please add or update the LilyPond direntries, do"
	@echo
	@echo "    install-info --info-dir=$(infodir) $(outdir)/lilypond.info"
	@echo
	@echo "For images in the INFO docs to work, do: "
	@echo
	@echo "    (cd $(infodir) && ln -sfT ../doc/lilypond/html/Documentation/user lilypond)"
	@echo "or add something like that to the postinstall script."
	@echo
else # installing directly into standard /usr/...
	-$(INSTALL) -d $(DESTDIR)$(infodir)
	-install-info --remove --info-dir=$(infodir) $(outdir)/lilypond.info
	-install-info --remove --info-dir=$(infodir) $(outdir)/lilypond-program.info
	-install-info --remove --info-dir=$(infodir) $(outdir)/lilypond-learning.info
	-install-info --remove --info-dir=$(infodir) $(outdir)/lilypond-internals.info
	-install-info --remove --info-dir=$(infodir) $(outdir)/music-glossary.info
	install-info --info-dir=$(infodir) $(outdir)/lilypond.info
	cd $(infodir) && ln -sfT $(webdir)/Documentation/user lilypond
endif # installing directly into standard /usr/...

local-uninstall-WWW:
	rm -f $(infodir)/lilypond

else # out!=www

# Cancel the default info generation rule that generates images:
$(outdir)/%.info: $(outdir)/%.texi

local-install-info: info
	-$(INSTALL) -d $(DESTDIR)$(package_infodir)
ifneq ($(patsubst %/local,%,$(DESTDIR)$(prefix)),/usr)
## Can not have absolute symlinks because some binary packages build schemes
## install files in nonstandard root.  Best we can do is to notify the
## builder or packager.
	@echo
	@echo "***************************************************************"
	@echo "Please add or update the LilyPond direntries, do"
	@echo
	@echo "    install-info --info-dir=$(infodir) out/lilypond.info"
	@echo
	@echo "For images in the INFO docs to work, do"
	@echo
	@echo "    make out=www install-info "
	@echo
	@echo "and read the extra instructions."
	@echo
else # installing directly into standard /usr/...
	-$(INSTALL) -d $(DESTDIR)$(infodir)
	-install-info --remove --info-dir=$(infodir) $(outdir)/lilypond.info
	-install-info --remove --info-dir=$(infodir) $(outdir)/lilypond-program.info
	-install-info --remove --info-dir=$(infodir) $(outdir)/lilypond-learning.info
	-install-info --remove --info-dir=$(infodir) $(outdir)/lilypond-internals.info
	-install-info --remove --info-dir=$(infodir) $(outdir)/music-glossary.info
	install-info --info-dir=$(infodir) $(outdir)/lilypond.info
	@echo
	@echo "***************************************************************"
	@echo "For images in the INFO docs to work, do"
	@echo
	@echo "    make out=www install-info "
	@echo
endif # installing into standard /usr/* root# installing into /usr/...

endif # out!=www

# All web targets, except info image symlinks and info docs are
# installed in non-recursing target from TOP-SRC-DIR
local-install-WWW: local-install-info
local-uninstall-WWW: local-uninstall-info

default:

local-clean:
	rm -f lilypond

local-help: extra-local-help

extra-local-help:
	@echo -e "\
  info        update info pages\n\
  ps          update PostScript documents\n\
  xml	      update Docbook xml documentation\n\
"

# Generic rule using % twice not possible?
# $(outdir)/%/%.html: $(outdir)/%.texi
$(outdir)/lilypond.texi: $(outdir)/lilypond-internals.texi
$(outdir)/lilypond.nexi: $(outdir)/lilypond-internals.texi

#
# Split manuals in HTML
#
$(outdir)/lilypond/index.html: $(OUT_PNG_IMAGES) $(OUT_EPS_IMAGES)
$(outdir)/lilypond-program/index.html: $(OUT_PNG_IMAGES) $(OUT_EPS_IMAGES)
$(outdir)/lilypond-learning/index.html: $(OUT_PNG_IMAGES) $(OUT_EPS_IMAGES)
$(outdir)/music-glossary/index.html: $(OUT_PNG_IMAGES) $(OUT_EPS_IMAGES)


#
# Manuals in one big HTML page
#
$(outdir)/lilypond-big-page.html: $(OUT_PNG_IMAGES)
$(outdir)/lilypond-program-big-page.html: $(OUT_PNG_IMAGES)
$(outdir)/lilypond-learning-big-page.html: $(OUT_PNG_IMAGES)

$(outdir)/lilypond.xml: $(outdir)/lilypond.texi
	mkdir -p $(dir $@)
	$(MAKEINFO) -I$(outdir) --output=$@ --docbook $<

$(outdir)/lilypond-internals/lilypond-internals.xml: $(outdir)/lilypond-internals.texi
	mkdir -p $(dir $@)
	$(MAKEINFO) --output=$(outdir)/lilypond-internals --docbook $<

$(outdir)/lilypond.pdf $(outdir)/lilypond-program.pdf: $(OUT_PDF_IMAGES) $(OUT_PNG_IMAGES)
$(outdir)/lilypond-learning.pdf $(outdir)/music-glossary.pdf: $(OUT_PDF_IMAGES) $(OUT_PNG_IMAGES)

$(outdir)/%.png: %.png
	convert -depth 8 -geometry 50x50% $< $@

$(outdir)/%.png: %.eps
	gs -dTextAlphaBits=4 -dGraphicsAlphaBits=4 -q -sOutputFile=$@ -sDEVICE=png16m -dEPSCrop -dNOPAUSE -f $< -c quit

$(outdir)/%.pdf: %.png
	convert -depth 8 $< $@

$(outdir)/%.pdf: %.eps
	gs -dAutoRotatePages=/None -sDEVICE=pdfwrite -dNOPAUSE -dBATCH -sOutputFile=$@ -dEPSCrop -c .setpdfwrite -f $<


DEEP_HTML_FILES =\
 $(outdir)/lilypond/index.html\
 $(outdir)/lilypond-internals/index.html\
 $(outdir)/music-glossary/index.html\
 $(outdir)/lilypond-program/index.html\
 $(outdir)/lilypond-learning/index.html

# Symlinks to refer to external source documents from split and non-split HTML
source-links = $(outdir)/source $(outdir)/lilypond/source $(outdir)/music-glossary/source $(outdir)/lilypond-program/source $(outdir)/lilypond-learning/source

$(outdir)/source:
	@rm -f $(@)
	ln -sf ../../ $(@)

$(outdir)/%/source:
	@rm -f $(@)
	mkdir -p $(dir $@)
	ln -sf ../../../ $(@)

local-WWW: $(HTML_FILES) $(DEEP_HTML_FILES)\
 $(datafiles) $(PDF_FILES) $(source-links) info

$(outdir)/%.bib: %.bib
	ln -f $< $@


# lilypond.texi deps
$(top-build-dir)/mf/$(outconfbase)/feta16list.ly:
	$(MAKE) -C $(top-src-dir)/mf

$(outdir)/lilypond.texi $(outdir)/lilypond-program.texi $(outdir)/lilypond-learning.texi: $(ITELY_FILES) $(ITEXI_FILES)
$(outdir)/lilypond.nexi $(outdir)/lilypond-program.nexi $(outdir)/lilypond-learning.nexi: $(ITELY_FILES) $(ITEXI_FILES)


# Rules for the automatically generated documentation

# There used to be a dependency on a dummy target, to force a rebuild
# of lilypond-internals every time.  however, this triggers
# compilation during install, which is a bad thing (tm).

$(outdir)/lilypond-internals.nexi $(outdir)/lilypond-internals.texi: $(LILYPOND_BINARY)
	cd $(outdir) && $(LILYPOND_BINARY) --verbose $(top-src-dir)/ly/generate-documentation
	rm -f $(outdir)/lilypond-internals.nexi
	-ln $(outdir)/lilypond-internals.texi $(outdir)/lilypond-internals.nexi


## unused
$(outdir)/interfaces.itexi: dummy
	cd $(outdir) && lilypond $(top-src-dir)/ly/generate-interface-doc
