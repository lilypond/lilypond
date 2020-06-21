depth = ../..

CSS_FILES = $(call src-wildcard,*.css)

OUT_CSS_FILES = $(CSS_FILES:%=$(outdir)/%)

STEPMAKE_TEMPLATES =

include $(depth)/make/stepmake.make

###########
### Targets

default:

ifeq ($(out),www)
local-WWW-1: $(OUT_CSS_FILES)
endif

#########
### Rules

$(outdir)/%.css: $(top-src-dir)/Documentation/css/%.css
	$(call ly_progress,Making,$@,(copy))
	rm -f $@ && cp $< $@
