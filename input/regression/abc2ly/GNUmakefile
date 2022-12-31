depth = ../../..

TEMPLATES = lilypond ly lysdoc texinfo

TEXI2HTML_FLAGS += --nomenu

ABC_FILES = $(call src-wildcard,*.abc)
COLLATED_FILES = $(sort ${ABC_FILES:%.abc=$(outdir)/%.ly})

include $(depth)/make/lilypond.make

$(outdir)/%.ly: %.abc
	$(call ly_progress,Making,$@,< abc)
	$(PYTHON) $(ABC2LY) --quiet -o $@ $<


TITLE=abc2ly test suite
