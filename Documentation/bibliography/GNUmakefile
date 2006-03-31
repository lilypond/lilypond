# Documentation/tex/Makefile

depth=../..

OUT_BIB_FILES = $(addprefix $(outdir)/, $(BIB_FILES))
BIB_FILES= $(call src-wildcard,*.bib)
EXTRA_DIST_FILES= $(BIB_FILES) $(call src-wildcard,*.bst)

STEPMAKE_TEMPLATES=tex documentation
LOCALSTEPMAKE_TEMPLATES=lilypond ly

include $(depth)/make/stepmake.make

dvi: $(DVI_FILES) $(OUT_BIB_FILES)

ps: $(PS_FILES)

# urg
default:

GENHTMLS = engraving colorado  computer-notation
OUTGENHTMLS = $(addprefix $(outdir)/, $(GENHTMLS:%=%.html))

local-WWW: $(addprefix $(outdir)/, $(BIB_FILES:.bib=.html) index.html)

$(outdir)/%.bib: %.bib
	ln -f $< $@

$(outdir)/%.html: %.bib
	BSTINPUTS=$(src-dir) $(PYTHON) $(depth)/buildscripts/bib2html.py -o $@ $<

local-clean:
	rm -f fonts.aux fonts.log feta*.tfm feta*.*pk
