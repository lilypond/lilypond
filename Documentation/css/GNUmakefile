depth = ../..

CSS_FILES = $(call src-wildcard,*.css)

EXTRA_DIST_FILES = $(CSS_FILES)

OUT_CSS_FILES = $(CSS_FILES:%=$(outdir)/%)

STEPMAKE_TEMPLATES = documentation

include $(depth)/make/stepmake.make

###########
### Targets

default:

ifeq ($(out),www)
local-WWW-1: $(OUT_CSS_FILES)
endif

#########
### Rules

$(outdir)/%.css: %.css
	ln -f $< $@
