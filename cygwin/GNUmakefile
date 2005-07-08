depth = ..

NAME = cygwin

MKNETRELS = $(wildcard mknetrel mknetrel.*[a-z])
EXTRA_DIST_FILES = changelog $(MKNETRELS) $(wildcard *.hint)

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

$(outdir)/%.sh: %.sh $(config_make)
	cat $< | sed $(sed-atvariables) > $@
	chmod 755 $@

default: $(OUT_POSTINSTALLS) $(OUT_POSTREMOVES) $(OUTIN_FILES) 

INSTALLATION_OUT_SUFFIXES=1

# Avoid collapsed directory constructs '//'
etc=$(patsubst %/, %, $(dir $(prefix)))/etc
INSTALLATION_DIR=$(etc)/postinstall
INSTALLATION_FILES=$(OUT_POSTINSTALLS)

INSTALLATION_OUT_DIR1=$(etc)/postremove
INSTALLATION_OUT_FILES1=$(OUT_POSTREMOVES)

endif

