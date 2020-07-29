depth = ../../..

STEPMAKE_TEMPLATES=texinfo
LOCALSTEPMAKE_TEMPLATES=lilypond ly lysdoc

TEXI2HTML_FLAGS += --nomenu

ABC_FILES = $(call src-wildcard,*.abc)
OUT_LY_FILES = $(sort ${ABC_FILES:%.abc=$(outdir)/%.ly})

include $(depth)/make/stepmake.make

$(outdir)/%.ly: %.abc
	$(call ly_progress,Making,$@,< abc)
	$(PYTHON) $(ABC2LY) --quiet -o $@ $<


TITLE=abc2ly test suite
