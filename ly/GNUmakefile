depth = ..

INI_FILES = $(LY_FILES)

INSTALLATION_DIR=$(local_lilypond_datadir)/ly
INSTALLATION_FILES=$(INI_FILES)

TEMPLATES = install lilypond ly

include $(depth)/make/lilypond.make

# The following runs a basic exercise, to make sure no backends crash
# in their default configs. This is particularly important for gs-api=#f,
# as we test/doc builds with gs-api=#t for performance reasons.
LILYPOND_FLAGS = -dpreview -dgs-api=\#f -l DEBUG

welcome_ps: Welcome_to_LilyPond.ly
	$(call ly_progress,Making,$(outdir)/$@,< $(notdir $<))
	$(buildscript-dir)/run-and-check.sh \
	    "$(LILYPOND_BINARY) $(LILYPOND_FLAGS) --formats=ps,pdf,png -o $(outdir)/$@ $<" \
	    "$(outdir)/$@.log"

welcome_svg: Welcome_to_LilyPond.ly
	$(call ly_progress,Making,$(outdir)/$@,< $(notdir $<))
	$(buildscript-dir)/run-and-check.sh \
	    "$(LILYPOND_BINARY) $(LILYPOND_FLAGS) --formats=svg -dbackend=svg -o $(outdir)/$@ $<" \
	    "$(outdir)/$@.log"

WELCOME_FILES = welcome_ps welcome_svg

welcome_cairo: Welcome_to_LilyPond.ly
	$(call ly_progress,Making,$(outdir)/$@,< $(notdir $<))
	$(buildscript-dir)/run-and-check.sh \
	    "$(LILYPOND_BINARY) $(LILYPOND_FLAGS) --formats=svg,ps,pdf,png -dbackend=cairo -o $(outdir)/$@ $<" \
	    "$(outdir)/$@.log"

ifneq ($(CAIRO_CFLAGS),)
WELCOME_FILES += welcome_cairo
endif


local-test: $(WELCOME_FILES)

.PHONY: welcome_svg welcome_ps

LY_FILES := $(call src-wildcard,*.ly)
TAGS_SOURCES += $(LY_FILES)

regex_ =
regex_ +=-l none
regex_ +=--regex='/[ \t]*\\?\(\([a-zA-Z]\([a-zA-Z]\|[-_][a-zA-Z]+\)*\([.]\([a-zA-Z]\([a-zA-Z]\|[-_][a-zA-Z]+\)*\)\)*\)\)[ \t]*=/\1/m'
regex_ +=--regex='/[ \t]*\\?\"\([^""]*\)\"[ \t]*=/\1/m'
regex_ +=--regex='/[ \t]*\#(define[ \t\n\v]+(*\([^ \t\n\v)(]+\)/\1/m'
regex_ +=--regex='/[ \t]*\#(define-public[ \t\n\v]+(*\([^ \t\n\v)(]+\)/\1/m'

ETAGS_FLAGS += $(regex_)
CTAGS_FLAGS += $(regex_)
