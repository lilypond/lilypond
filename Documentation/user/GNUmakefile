depth=../..

LATEX_FILES =$(call src-wildcard,*.latex)


EXTRA_DIST_FILES = $(LATEX_FILES) $(IMAGES) $(EPS_ILLUSTRATIONS)
EXTRA_DIST_FILES += README.txt writing-texinfo.txt policy.txt writing-sections.txt

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

INFO_DOCS = lilypond lilypond-internals music-glossary lilypond-program lilypond-learning
INFO_FILES = $(INFO_DOCS:%=$(outdir)/%.info)

STEPMAKE_TEMPLATES=tex texinfo omf documentation
#TEXI2DVI_FLAGS = -E
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
# Viewawble with a recent Emacs, doing: M-x info out-www/lilypond.info

# Cancel the special, non-image info generation rule that skips images:
$(outdir)/%.info: $(outdir)/%.nexi

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
	@echo "    install-info --info-dir=$(infodir) $(outdir)/lilypond.info"
	@echo
	@echo "For images in the INFO docs to work, do: "
	@echo
	@echo "    (cd $(package_infodir) && ln -sf ../../doc/lilypond/Documentation/user/*png .)"
	@echo "or add something like that to the postinstall script."
	@echo
else # installing directly into standard /usr/...
	-$(INSTALL) -d $(DESTDIR)$(package_infodir)
	-install-info --remove --info-dir=$(infodir) $(outdir)/lilypond.info
	-install-info --remove --info-dir=$(infodir) $(outdir)/lilypond-program.info
	-install-info --remove --info-dir=$(infodir) $(outdir)/lilypond-learning.info
	install-info --info-dir=$(infodir) $(outdir)/lilypond.info
	(cd $(package_infodir) && ln -sf $(webdir)/Documentation/user/*png .)
endif # installing directly into standard /usr/...

local-uninstall-WWW:
	rm -f $(package_infodir)/*.png

else # out!=www

# Cancel the default info generation rule that generates images:
$(outdir)/%.info: # $(outdir)/%.texi

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
	-$(INSTALL) -d $(DESTDIR)$(package_infodir)
	-install-info --remove --info-dir=$(infodir) $(outdir)/lilypond.info
	-install-info --remove --info-dir=$(infodir) $(outdir)/lilypond-program.info
	-install-info --remove --info-dir=$(infodir) $(outdir)/lilypond-learning.info
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

#
# The split user manual
#
$(outdir)/lilypond/index.html: $(outdir)/lilypond.texi $(OUT_PNG_IMAGES) $(OUT_EPS_IMAGES)
	mkdir -p $(dir $@)
	$(MAKEINFO) -I$(outdir) --output=$(outdir)/lilypond --css-include=$(top-src-dir)/Documentation/texinfo.css --html $<
# we /might/ switch to texi2html if it can be fixed:
#	echo "*************************************"
#	echo $<
#	cd $(outdir)
#	texi2html --output=$(outdir)/lilypond --css-include=$(top-src-dir)/Documentation/texinfo.css $<
	find $(outdir)/lilypond/ -name '*'.png -o -name '*'.ly | xargs rm -f
# symbolic links to save space
	(cd $(outdir)/lilypond/ ; ln -sf ../*.png ../*.ly . )

#
# One big page user manual
#
$(outdir)/lilypond-big-page.html: $(outdir)/lilypond.texi $(OUT_PNG_IMAGES)
	$(MAKEINFO) -I$(outdir) --output=$@ --css-include=$(top-src-dir)/Documentation/texinfo.css --html --no-split -D bigpage --no-headers $<

#
# The split program usage
#
$(outdir)/lilypond-program/index.html: $(outdir)/lilypond-program.texi $(OUT_PNG_IMAGES) $(OUT_EPS_IMAGES)
	mkdir -p $(dir $@)
	$(MAKEINFO) -I$(outdir) --output=$(outdir)/lilypond-program --css-include=$(top-src-dir)/Documentation/texinfo.css --html $<
	find $(outdir)/lilypond-program/ -name '*'.png -o -name '*'.ly | xargs rm -f
# symbolic links to save space
	(cd $(outdir)/lilypond-program/ ; ln -sf ../*.png ../*.ly . )

#
# The Learning Manual
#
$(outdir)/lilypond-learning/index.html: $(outdir)/lilypond-learning.texi $(OUT_PNG_IMAGES) $(OUT_EPS_IMAGES)
	mkdir -p $(dir $@)
	$(MAKEINFO) -I$(outdir) --output=$(outdir)/lilypond-learning --css-include=$(top-src-dir)/Documentation/texinfo.css --html $<
	find $(outdir)/lilypond-learning/ -name '*'.png -o -name '*'.ly | xargs rm -f
# symbolic links to save space
	(cd $(outdir)/lilypond-learning/ ; ln -sf ../*.png ../*.ly . )


#
# One big page program usage
#
$(outdir)/lilypond-program-big-page.html: $(outdir)/lilypond-program.texi $(OUT_PNG_IMAGES)
	$(MAKEINFO) -I$(outdir) --output=$@ --css-include=$(top-src-dir)/Documentation/texinfo.css --html --no-split -D bigpage --no-headers $<

#
# One big page learning manual
#
$(outdir)/lilypond-learning-big-page.html: $(outdir)/lilypond-learning.texi $(OUT_PNG_IMAGES)
	$(MAKEINFO) -I$(outdir) --output=$@ --css-include=$(top-src-dir)/Documentation/texinfo.css --html --no-split -D bigpage --no-headers $<


#
# The split internals reference
#
$(outdir)/lilypond-internals/index.html: $(outdir)/lilypond-internals.texi
	mkdir -p $(dir $@)
	$(MAKEINFO) --output=$(outdir)/lilypond-internals --css-include=$(top-src-dir)/Documentation/texinfo.css --html $<

#
# One big page internals reference
#
$(outdir)/lilypond-internals-big-page.html: $(outdir)/lilypond-internals.texi
	$(MAKEINFO) --output=$@ --css-include=$(top-src-dir)/Documentation/texinfo.css --html --no-split -D bigpage --no-headers $<

#
# The split glossary
#
$(outdir)/music-glossary/index.html: $(outdir)/music-glossary.texi
	mkdir -p $(dir $@)
	$(MAKEINFO) --output=$(outdir)/music-glossary --css-include=$(top-src-dir)/Documentation/texinfo.css --html $<
	find $(outdir)/music-glossary/ -name '*'.png -o -name '*'.ly | xargs rm -f
# symbolic links to save space
	(cd $(outdir)/music-glossary/ ; ln -sf ../*.png ../*.ly . )

$(outdir)/lilypond.xml: $(outdir)/lilypond.texi
	mkdir -p $(dir $@)
	$(MAKEINFO) -I$(outdir) --output=$@ --docbook $<

$(outdir)/lilypond-internals/lilypond-internals.xml: $(outdir)/lilypond-internals.texi
	mkdir -p $(dir $@)
	$(MAKEINFO) --output=$(outdir)/lilypond-internals --docbook $<

$(outdir)/lilypond.pdf: $(OUT_PDF_IMAGES) $(OUT_PNG_IMAGES)

$(outdir)/lilypond-program.pdf: $(OUT_PDF_IMAGES) $(OUT_PNG_IMAGES)

$(outdir)/lilypond-learning.pdf: $(OUT_PDF_IMAGES) $(OUT_PNG_IMAGES)

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

$(outdir)/lilypond/source:
	@rm -f $(@)
	mkdir -p $(outdir)/lilypond
	ln -sf ../../../ $(@)

$(outdir)/music-glossary/source:
	@rm -f $(@)
	mkdir -p $(outdir)/music-glossary
	ln -sf ../../../ $(@)

$(outdir)/lilypond-program/source:
	@rm -f $(@)
	mkdir -p $(outdir)/lilypond-program
	ln -sf ../../../ $(@)

$(outdir)/lilypond-learning/source:
	@rm -f $(@)
	mkdir -p $(outdir)/lilypond-learning
	ln -sf ../../../ $(@)

local-WWW: $(HTML_FILES) $(DEEP_HTML_FILES)\
 $(datafiles) $(PDF_FILES) $(source-links) info info-dir

info-dir:
	$(SHELL) $(buildscript-dir)/install-info-html.sh --dir=$(outdir) lilypond lilypond-internals music-glossary lilypond-program lilypond-learning


$(outdir)/%.bib: %.bib
	ln -f $< $@


# lilypond.texi deps
$(top-build-dir)/mf/$(outconfbase)/feta16list.ly:
	$(MAKE) -C $(top-src-dir)/mf

$(outdir)/lilypond.texi: $(ITELY_FILES) $(ITEXI_FILES)
$(outdir)/lilypond.nexi: $(ITELY_FILES) $(ITEXI_FILES)

# Prevent building music-glossary.texi from default target
$(outdir)/music-glossary.nexi:

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
