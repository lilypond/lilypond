depth = ..

NAME = cygwin

EXTRA_DIST_FILES = changelog mknetrel $(wildcard *.hint)

SEXECUTABLES=bug-lilypond-cygwin lily-wins
STEPMAKE_TEMPLATES = install install-out script

include $(depth)/make/stepmake.make

# For Cygwin builds only
ifneq ($(CYGWIN_BUILD),)

include $(stepdir)/executable-targets.make

POSTINSTALLS=$(wildcard postinstall*.sh)
OUT_POSTINSTALLS=$(POSTINSTALLS:%=$(outdir)/%)

POSTREMOVES=$(wildcard postremove*.sh)
OUT_POSTREMOVES=$(POSTREMOVES:%=$(outdir)/%)

foe:
	echo pi: $(POSTINSTALLS)
	echo opi: $(OUT_POSTINSTALLS)
	echo pr: $(POSTREMOVES)
	echo opr: $(OUT_POSTREMOVES)

PROFILES=$(wildcard *-profile.sh) lilypond-profile.sh
OUT_PROFILES=$(PROFILES:%=$(outdir)/%)

# profiles
$(outdir)/%.sh: %.sh $(config_make)
	cat $< | sed $(sed-atvariables) > $@
	chmod 755 $@

# Get profile from buildscripts
$(outdir)/lilypond-profile.sh: $(outdir)/../$(depth)/buildscripts/$(outconfbase)/lilypond-profile
	cp $< $@
	chmod 755 $@

default: $(OUT_POSTINSTALLS) $(OUT_POSTREMOVES) $(OUT_PROFILES) $(OUTIN_FILES) 

INSTALLATION_OUT_SUFFIXES=1 2 # 3

# Avoid collapsed directory constructs '//'
etc=$(patsubst %/, %, $(dir $(prefix)))/etc
INSTALLATION_DIR=$(etc)/postinstall
INSTALLATION_FILES=$(OUT_POSTINSTALLS)

INSTALLATION_OUT_DIR1=$(etc)/profile.d
INSTALLATION_OUT_FILES1=$(OUT_PROFILES)

INSTALLATION_OUT_DIR2=$(etc)/postremove
INSTALLATION_OUT_FILES2=$(OUT_POSTREMOVES)

# INSTALLATION_OUT_DIR3=$(local_lilypond_datadir)/tex
# INSTALLATION_OUT_FILES3=$(shell kpsewhich geometry.sty)

endif

