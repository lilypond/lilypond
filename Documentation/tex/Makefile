# Documentation/tex/Makefile

depth=../..

include $(depth)/make/Stepmake.make 
include $(stepdir)/Documentation.vars.make
include $(depth)/make/Lilypond.make
include $(stepdir)/Documentation_rules.make

DATA_FILES = $(wildcard *.data)
datafiles = $(addprefix $(outdir)/,$(DATA_FILES:.data=.html))
DOC_FILES = $(wildcard *.doc)
DVI_FILES = $(addprefix $(outdir)/,$(DOC_FILES:.doc=.dvi) $(YO_FILES:.yo=.dvi))
OUTDOC_FILES = $(addprefix $(outdir)/, $(DOC_FILES))
EL_FILES = $(wildcard *.el)
BIB_FILES= $(wildcard *.bib)
EXTRA_DIST_FILES= $(BIB_FILES) $(DOC_FILES) $(DATA_FILES) $(EL_FILES)
HTML_FILES = $(addprefix $(outdir)/, $(YO_FILES:.yo=.html))
PS_FILES = $(DVI_FILES:.dvi=.ps)


dvi: $(DVI_FILES)

ps: $(PS_FILES)

# urg
default:

GENHTMLS = engraving colorado vocabulary computer-notation
OUTGENHTMLS = $(addprefix $(outdir)/, $(GENHTMLS:%=%.html))


$(outdir)/glossary.dvi: $(outdir)/vocabulary.tex 
$(outdir)/glossary.html: $(OUTGENHTMLS)

local-WWW: $(HTML_FILES) $(OUTDOC_FILES:.doc=.ps.gz) $(OUTYO_FILES:.yo=.latex) $(OUTYO_FILES:.yo=.ps.gz) $(addprefix $(outdir)/, $(BIB_FILES:.bib=.html)) $(datafiles)
	$(PYTHON) $(step-bindir)/ls-latex.py --package=$(topdir) --title 'LaTeX documents about LilyPond' \
	  $(YO_FILES) $(OUTYO_FILES:.yo=.latex) $(BIB_FILES) $(DOC_FILES) \
	  | sed "s!$(outdir)/!!g" > $(outdir)/index.html
	  $(PYTHON) $(step-bindir)/add-html-footer.py --package=$(topdir) $(outdir)/index.html

$(outdir)/%.bib: %.bib
	ln -f $< $@

$(outdir)/%.html: %.bib
	bib2html $< $@

# Yeah right: 
# make -k out/mudela.dvi => cp -f out/vocabulary.tex out/mudela.dvi

#out/%: $(outdir)/%
#	cp -f $< $@
