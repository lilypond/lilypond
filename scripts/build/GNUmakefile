depth = ../..

STEPMAKE_TEMPLATES=script install po

# need env settings to run output-distance --test
LOCALSTEPMAKE_TEMPLATES=lilypond

include $(depth)/make/stepmake.make

# Should we install these? This should be handled by sysadmin or
# packager but if she forgets...
#INSTALLATION_OUT_SUFFIXES=1
#INSTALLATION_OUT_DIR1=$(local_lilypond_datadir)/scripts
#INSTALLATION_OUT_FILES1=$(outdir)/lilypond-login $(outdir)/lilypond-profile

all: $(INSTALLATION_FILES)

local-test: output-distance.py run-and-check.sh
	$(call ly_progress,testing,$@)
	$(src-dir)/run-and-check.sh "$(PYTHON) $< --test --output-dir=$(outdir)" $(outdir)/output-distance-test.log
