depth = ../..

STEPMAKE_TEMPLATES=documentation texinfo tex
LOCALSTEPMAKE_TEMPLATES=lilypond ly lysdoc

include $(depth)/make/stepmake.make
TITLE=LilyPond Regression Tests


test:
	$(MAKE) out=test WWW ANTI_ALIAS_FACTOR=1


CHECK_SOURCE=$(HOME)/vc/gub/target/linux-x86/build/lilypond-master-git.sv.gnu.org-lilypond.git/input/regression/out-test/
RESULT_DIR=$(top-build-dir)/out/test-results/
check:
	rm -rf $(RESULT_DIR)
	mkdir -p $(RESULT_DIR)
	$(PYTHON) $(buildscript-dir)/output-distance.py --output-dir $(RESULT_DIR) $(CHECK_SOURCE) out-test/
