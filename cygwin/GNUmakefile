depth = ..

NAME = cygwin

EXTRA_DIST_FILES = changelog mknetrel $(wildcard *.sh *.hint)

include $(depth)/make/stepmake.make

# For cygwin builds only
ifneq ($(CYGWIN_BUILD),)

STEPMAKE_TEMPLATES = install install-out

POST_INSTALLS=$(wildcard post-*.sh)
OUT_POST_INSTALLS=$(POST_INSTALLS:%=$(outdir)/%)

PROFILES=$(wildcard *-profile.sh) lilypond-profile.sh
OUT_PROFILES=$(PROFILES:%=$(outdir)/%)

# profiles
$(outdir)/%.sh: %.sh
	cat $< | sed $(sed-atvariables) > $@
	chmod 755 $@

# Urg
$(outdir)/lilypond-profile.sh: $(outdir)/../$(depth)/buildscripts/$(outconfbase)/lilypond-profile
	cp $< $@
	chmod 755 $@

default: $(OUT_POST_INSTALLS) $(OUT_PROFILES) $(OUTIN_FILES)

INSTALLATION_OUT_SUFFIXES=1 2

# URG.
# By popular demand,
# LilyPond on windows is configured with --prefix=/usr/lilypond-x.y.x
# The cygwin profile.d dir, however, is in /etc

# avoid collapsed directory constructs '//'
#etc=$(dir $(patsubst %/, %, $(dir $(prefix)/)))etc
etc=$(patsubst %/, %, $(dir $(prefix)))/etc
INSTALLATION_DIR=$(etc)/postinstall
INSTALLATION_FILES=$(OUT_POST_INSTALLS)

INSTALLATION_OUT_DIR1=$(etc)/profile.d
INSTALLATION_OUT_FILES1=$(OUT_PROFILES)

INSTALLATION_OUT_DIR2=$(local_lilypond_datadir)/tex
INSTALLATION_OUT_FILES2=$(shell kpsewhich geometry.sty)

endif

