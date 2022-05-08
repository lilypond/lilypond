depth = ../..

# need env settings to run output-distance --test
TEMPLATES=lilypond

include $(depth)/make/lilypond.make

local-test: output-distance.py run-and-check.sh
	$(call ly_progress,testing,$<)
	$(src-dir)/run-and-check.sh "$(PYTHON) $< --test --output-dir=$(outdir)" $(outdir)/output-distance-test.log
