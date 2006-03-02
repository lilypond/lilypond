depth = ..

NAME = cygwin

MKNETRELS = mknetrel $(call src-wildcard,mknetrel.*[a-z])
EXTRA_DIST_FILES = changelog $(MKNETRELS) $(call src-wildcard,*.hint)

SEXECUTABLES=bug-lilypond-cygwin lily-wins
STEPMAKE_TEMPLATES = install install-out script

include $(depth)/make/stepmake.make

# For Cygwin builds only
ifneq ($(CYGWIN_BUILD),)

include $(stepdir)/executable-targets.make

POSTINSTALLS=$(call src-wildcard,postinstall*.sh)
OUT_POSTINSTALLS=$(POSTINSTALLS:%=$(outdir)/%)

$(outdir)/%.sh: %.sh $(config_make)
	cat $< | sed $(sed-atvariables) > $@
	chmod 755 $@

default: $(OUT_POSTINSTALLS) $(OUTIN_FILES)

# Avoid collapsed directory constructs '//'
etc=$(patsubst %/,%, $(dir $(prefix)))/etc
INSTALLATION_OUT_DIR=$(etc)/postinstall
INSTALLATION_OUT_FILES=$(OUT_POSTINSTALLS)

endif

