depth = ../../..

TEMPLATES = lilypond ly lysdoc 

ABC_FILES = $(call src-wildcard,*.abc)
COLLATED_FILES = \
  AAA-intro-regression.tely \
  $(sort ${ABC_FILES:%.abc=$(outdir)/%.ly})

include $(depth)/make/lilypond.make

$(outdir)/%.ly: %.abc
	$(call ly_progress,Making,$@,< abc)
	$(PYTHON) $(ABC2LY) --beams --quiet -o $@ $<


TITLE=abc2ly test suite
