depth = ..

INI_FILES = $(LY_FILES)

INSTALLATION_DIR=$(local_lilypond_datadir)/ly
INSTALLATION_FILES=$(INI_FILES)

STEPMAKE_TEMPLATES=install
LOCALSTEPMAKE_TEMPLATES=lilypond ly

include $(depth)/make/stepmake.make

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

local-test: $(WELCOME_FILES)

.PHONY: welcome_svg welcome_ps
