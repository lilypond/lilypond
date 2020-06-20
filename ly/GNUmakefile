depth = ..

INI_FILES = $(LY_FILES)
EXTRA_DIST_FILES = $(SCM_FILES)

INSTALLATION_DIR=$(local_lilypond_datadir)/ly
INSTALLATION_FILES=$(INI_FILES)

STEPMAKE_TEMPLATES=install
LOCALSTEPMAKE_TEMPLATES=lilypond ly

include $(depth)/make/stepmake.make

# Note: The test files are written to $(outdir), even though the rules don't
# specify it as their output. This has the advantage that the commands are
# always run -- .PHONY requires explicit rules, so this doesn't work.
LILYPOND_FLAGS = -dgs-api=\#f -l DEBUG
welcome.%: Welcome_to_LilyPond.ly
	$(call ly_progress,Making,$(outdir)/$@,< $(notdir $<))
	$(buildscript-dir)/run-and-check.sh \
	    "$(LILYPOND_BINARY) $(LILYPOND_FLAGS) --$* -o $(outdir)/welcome $<" \
	    "$@.log"

LILYPOND_EPS_FLAGS = $(LILYPOND_FLAGS) -dbackend=eps --formats=pdf,png
welcome_eps.pdf: Welcome_to_LilyPond.ly
	$(call ly_progress,Making,$(outdir)/$@,< $(notdir $<))
	$(buildscript-dir)/run-and-check.sh \
	    "$(LILYPOND_BINARY) $(LILYPOND_EPS_FLAGS) -o $(outdir)/welcome_eps $<" \
	    "$@.log"

WELCOME_FILES = welcome.pdf welcome.png welcome.svg welcome_eps.pdf

local-test: $(WELCOME_FILES)
