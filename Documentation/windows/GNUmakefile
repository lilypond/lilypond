
depth=../..

EXTRA_DIST_FILES = LilyPond.ico $(wildcard *.sh *.patch)

STEPMAKE_TEMPLATES=documentation texinfo install install-out

include $(depth)/make/stepmake.make 

default: local-doc

local-WWW:

# For cygwin builds only
target=$(shell gcc -dumpmachine)
ifeq ($(target),i686-pc-cygwin)

POST_INSTALLS=$(wildcard post-*.sh)
OUT_POST_INSTALLS=$(POST_INSTALLS:%=$(outdir)/%)

# lily-*: rename to '-profile' or so
PROFILES=$(wildcard *lily-*.sh) lilypond-profile.sh
OUT_PROFILES=$(PROFILES:%=$(outdir)/%)

##as2text.scm
PYTHON_WRAPPERS=\
 abc2ly\
 convert-ly\
 etf2ly\
 lilypond-book\
 ly2dvi\
 mup2ly\
 musedata2ly\
 pmx2ly\
 update-lily\
#

OUT_PYTHON_WRAPPERS=$(PYTHON_WRAPPERS:%=$(outdir)/%)

TEX_WRAPPERS=dvips latex mktextfm tex yap
OUT_TEX_WRAPPERS=$(TEX_WRAPPERS:%=$(outdir)/%)

# profiles
$(outdir)/%.sh: %.sh
	cat $< | sed $(sed-atvariables) > $@
	chmod 755 $@

# Urg
$(outdir)/lilypond-profile.sh: $(outdir)/../$(depth)/buildscripts/out/lilypond-profile
	cp $< $@
	chmod 755 $@

# python wrappers
$(outdir)/%: python-wrapper.sh
	cat $< | sed $(sed-atvariables) -e "s!@name@!$(*F)!g" > $@
	chmod 755 $@

ATVARIABLES += OPTIONS

# tex wrappers
#$(outdir)/%: %-wrapper.sh
#	cat $< | sed $(sed-atvariables) -e "s!@name@!$(*F)!g" > $@
#	chmod 755 $@

$(outdir)/latex: tex-wrapper.sh
	cat $< | sed $(sed-atvariables) -e "s!@name@!latex!g" > $@
	chmod 755 $@

$(outdir)/dvips: tex-wrapper.sh
	cat $< | sed $(sed-atvariables) -e "s!@name@!dvips!g" > $@
	chmod 755 $@

MKTEXFTM_OPTIONS=--dest-dir \"$$(cygpath -w \"\"/usr/lilypond/share/lilypond/tfm\"\")\"
$(outdir)/mktextfm: tex-wrapper.sh
	cat $< | sed -e 's!@OPTIONS@!$(MKTEXFTM_OPTIONS)!' $(sed-atvariables) -e "s!@name@!maketfm!g" > $@
	chmod 755 $@

$(outdir)/tex: tex-wrapper.sh
	cat $< | sed $(sed-atvariables) -e "s!@name@!tex!g" > $@
	chmod 755 $@

$(outdir)/yap: tex-wrapper.sh
	cat $< | sed $(sed-atvariables) -e "s!@name@!yap!g" > $@
	chmod 755 $@

default: $(OUT_PYTHON_WRAPPERS) $(OUT_TEX_WRAPPERS) $(OUT_POST_INSTALLS) $(OUT_PROFILES)

# urg: change suffixes before overwriting python scripts

INSTALLATION_OUT_SUFFIXES=1 2 3 4

# URG.
# By popular demand,
# LilyPond on windows is configured with --prefix=/usr/lilypond-x.y.x
# The cygwin profile.d dir, however, is in /etc

INSTALLATION_DIR=$(shell dirname $(shell dirname $(prefix)))/etc/postinstall
INSTALLATION_FILES=$(OUT_POST_INSTALLS)

INSTALLATION_OUT_DIR1=$(shell dirname $(shell dirname $(prefix)))/etc/profile.d
INSTALLATION_OUT_FILES1=$(OUT_PROFILES)

INSTALLATION_OUT_DIR2=$(prefix)/wrappers
INSTALLATION_OUT_FILES2=$(OUT_PYTHON_WRAPPERS) $(OUT_TEX_WRAPPERS)

INSTALLATION_OUT_DIR3=$(datadir)/tex
INSTALLATION_OUT_FILES3=$(shell kpsewhich geometry.sty)

INSTALLATION_OUT_DIR4=$(datadir)/tfm
INSTALLATION_OUT_FILES4=$(shell kpsewhich cmr10.tfm)

else

local-install:
	@echo skipping

local-install-outfiles:
	@echo skipping

endif
