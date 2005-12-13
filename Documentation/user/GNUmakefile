depth=../..

LATEX_FILES =$(wildcard *.latex)

# todo: add latex.
DVI_FILES = $(TELY_FILES:%.tely=$(outdir)/%.dvi)

EXTRA_DIST_FILES= $(LATEX_FILES) $(IMAGES)
IMAGES=$(wildcard *.png)

OUT_EPS_IMAGES=$(IMAGES:%.png=$(outdir)/%.eps)
OUT_PNG_IMAGES=$(IMAGES:%=$(outdir)/%)
OUT_TEXI_FILES=$(ITEXI_FILES:%.itexi=$(outdir)/%.texi)\
 $(ITELY_FILES:%.itely=$(outdir)/%.texi)
HTML_FILES = $(TELY_FILES:%.tely=$(outdir)/%.html)\
 $(outdir)/lilypond-internals.html

PS_FILES = $(DVI_FILES:.dvi=.ps)
PDF_FILES = $(DVI_FILES:.dvi=.pdf)

PS_GZ_FILES= $(addsuffix .gz, $(PS_FILES))

INFO_DOCS = lilypond lilypond-internals music-glossary
INFO_FILES = $(INFO_DOCS:%=$(outdir)/%.info)

STEPMAKE_TEMPLATES=tex texinfo omf documentation

OMF_FILES += $(outdir)/lilypond-internals.html.omf

LOCALSTEPMAKE_TEMPLATES=lilypond ly


TEXINPUTS=$(topdir)/tex::
export TEXINPUTS

include $(depth)/make/stepmake.make 

dvi: $(DVI_FILES)

ps: $(PS_FILES)

info: $(INFO_FILES)

pathsettings:
	@echo export PATH=$(PATH)
	@echo export LILYPONDPREFIX=$(LILYPONDPREFIX)
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
	@echo "    (cd $(package_infodir) && ln -sf ../../doc/lilypond/Documentation/user/out-www/*png .)"
	@echo "or add something like that to the postinstall script."
	@echo
else
	-$(INSTALL) -d $(DESTDIR)$(package_infodir)
	-install-info --remove --info-dir=$(infodir) $(outdir)/lilypond.info
	install-info --info-dir=$(infodir) $(outdir)/lilypond.info
	(cd $(lilypond_infodir) && ln -sf $(webdir)/Documentation/user/out-www/*png .)
endif

local-uninstall-WWW:
	rm -f $(package_infodir)/*.png

else

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
else
	-$(INSTALL) -d $(DESTDIR)$(package_infodir)
	-install-info --remove --info-dir=$(infodir) $(outdir)/lilypond.info
	install-info --info-dir=$(infodir) $(outdir)/lilypond.info
	@echo
	@echo "***************************************************************"
	@echo "For images in the INFO docs to work, do"
	@echo
	@echo "    make out=www install-info "
	@echo
endif


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

#
# The split user manual
#
$(outdir)/lilypond/index.html: $(outdir)/lilypond.texi $(OUT_PNG_IMAGES)
	mkdir -p $(dir $@)
	$(MAKEINFO) -I$(outdir) --output=$(outdir)/lilypond --css-include=$(abs-srcdir)/Documentation/texinfo.css --html $<
	find $(outdir)/lilypond/ -name '*'.png -o -name '*'.ly | xargs rm -f
# symbolic links to save space 
	(cd $(outdir)/lilypond/ ; ln -sf ../*.png ../*.ly . )

#
# One big page manual
#
$(outdir)/lilypond.html: $(outdir)/lilypond.texi
	$(MAKEINFO) -I$(outdir) --output=$@ --css-include=$(abs-srcdir)/Documentation/texinfo.css --html --no-split --no-headers $<

#
# The split internals reference
#
$(outdir)/lilypond-internals/index.html: $(outdir)/lilypond-internals.texi
	mkdir -p $(dir $@)
	$(MAKEINFO) --output=$(outdir)/lilypond-internals --css-include=$(abs-srcdir)/Documentation/texinfo.css --html $<

#
# One big page internals reference
#
$(outdir)/lilypond-internals.html: $(outdir)/lilypond-internals.texi
	$(MAKEINFO) --output=$@ --css-include=$(abs-srcdir)/Documentation/texinfo.css --html --no-split --no-headers $<

#
# The split glossary
#
$(outdir)/music-glossary/index.html: $(outdir)/music-glossary.texi
	mkdir -p $(dir $@)
	$(MAKEINFO) --output=$(outdir)/music-glossary --css-include=$(abs-srcdir)/Documentation/texinfo.css --html $<
	find $(outdir)/music-glossary/ -name '*'.png -o -name '*'.ly | xargs rm -f
# symbolic links to save space 
	(cd $(outdir)/music-glossary/ ; ln -sf ../*.png ../*.ly . )

$(outdir)/lilypond.xml: $(outdir)/lilypond.texi 
	mkdir -p $(dir $@)
	$(MAKEINFO) -I$(outdir) --output=$@ --docbook $<

$(outdir)/lilypond-internals/lilypond-internals.xml: $(outdir)/lilypond-internals.texi
	mkdir -p $(dir $@)
	$(MAKEINFO) --output=$(outdir)/lilypond-internals --docbook $<

$(outdir)/lilypond.dvi: $(OUT_EPS_IMAGES) $(OUT_PNG_IMAGES)

$(outdir)/%.png: %.png
	convert -geometry 50x50% $< $@

$(outdir)/%.eps: %.png
	convert $< $@

DEEP_HTML_FILES =\
 $(outdir)/lilypond/index.html\
 $(outdir)/lilypond-internals/index.html\
 $(outdir)/music-glossary/index.html

# Symlinks to refer to external source documents from split and non-split HTML
source-links = $(outdir)/source $(outdir)/lilypond/source $(outdir)/music-glossary/source

$(outdir)/source:
	@rm -f $(@)
	ln -sf ../../ $(@)

$(outdir)/lilypond/source:
	@rm -f $(@)
	ln -sf ../../../ $(@)

$(outdir)/music-glossary/source:
	@rm -f $(@)
	ln -sf ../../../ $(@)

local-WWW: $(HTML_FILES) $(DEEP_HTML_FILES)\
 $(datafiles) $(PDF_FILES) $(source-links) info info-dir

local-WWW-clean: deep-WWW-clean

deep-WWW-clean:
	rm -rf $(outdir)/lilypond $(outdir)/lilypond-internals

info-dir:
	$(SHELL) $(buildscript-dir)/install-info-html.sh --dir=$(outdir) lilypond lilypond-internals music-glossary


$(outdir)/%.bib: %.bib
	ln -f $< $@

local-clean:
	rm -f fonts.aux fonts.log feta*.tfm feta*.*pk 
	rm -rf $(outdir)/lilypond $(outdir)/lilypond-internals $(outdir)/music-glossary

# lilypond.texi deps
$(builddir)/mf/$(outconfbase)/feta16list.ly:
	$(MAKE) -C $(topdir)/mf

$(outdir)/lilypond.texi: $(ITELY_FILES) $(ITEXI_FILES) 
$(outdir)/lilypond.nexi: $(ITELY_FILES) $(ITEXI_FILES)

# Prevent building music-glossary.texi from default target
$(outdir)/music-glossary.nexi:

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

$(outdir)/programming-interface.texi:
	cp dummy-interfaces.itexi $@
endif

local-clean: local-delete

local-delete:
	-for i in $$(seq 0 9); do \
		for j in dvi eps eps.bbox eps.trans eps.trans.ps ps log png tex; do \
			rm -f $(outdir)/lily-$$i*$$j; \
		done; \
	done
	-rm -f $(outdir)/*
