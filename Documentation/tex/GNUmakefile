# Documentation/tex/Makefile

depth=../..

DATA_FILES = $(wildcard *.data)
datafiles = $(addprefix $(outdir)/,$(DATA_FILES:.data=.html))
DOC_FILES = $(wildcard *.doc)
DVI_FILES = $(addprefix $(outdir)/,$(DOC_FILES:.doc=.dvi) $(YO_FILES:.yo=.dvi))
OUT_BIB_FILES = $(addprefix $(outdir)/, $(BIB_FILES))

# just include me
YO_URG_FILES = $(wildcard *.yo-urg)

OUTDOC_FILES = $(addprefix $(outdir)/, $(DOC_FILES))
EL_FILES = $(wildcard *.el)
BIB_FILES= $(wildcard *.bib)
EXTRA_DIST_FILES= $(BIB_FILES) $(DOC_FILES) $(DATA_FILES) $(EL_FILES) $(YO_URG_FILES)
HTML_FILES = $(addprefix $(outdir)/, $(YO_FILES:.yo=.html))
PS_FILES = $(DVI_FILES:.dvi=.ps)

STEPMAKE_TEMPLATES=tex documentation
LOCALSTEPMAKE_TEMPLATES=lilypond mudela

export BIBINPUTS:=$(shell pwd)//$(PATHSEP)$(BIBINPUTS)
include $(depth)/make/stepmake.make 






dvi: $(OUT_BIB_FILES) $(DVI_FILES)


ps: $(PS_FILES)

# urg
default:

GENHTMLS = engraving colorado glossary computer-notation
OUTGENHTMLS = $(addprefix $(outdir)/, $(GENHTMLS:%=%.html))

#urg should generalise and move Lilypond -> StepMake
# URG.  Lilypond specific. Move out. 
$(outdir)/%.html: %.data $(depth)/VERSION
	$(PYTHON) $(step-bindir)/table-to-html.py --columns=7 --linesep='' --package=$(topdir) -o $@ $<
	$(PYTHON) $(step-bindir)/add-html-footer.py --package=$(topdir) $@

$(outdir)/%.tex: %.data $(depth)/VERSION
	$(PYTHON) $(step-bindir)/table-to-html.py --columns=7 --linesep='' --package=$(topdir) -o $@ --latex $<


$(outdir)/glossary.dvi: $(outdir)/glossary-table.tex
$(outdir)/glossary.html: $(outdir)/glossary-table.html


local-WWW: $(HTML_FILES) $(OUTDOC_FILES:.doc=.ps.gz) $(OUTYO_FILES:.yo=.latex) $(OUTYO_FILES:.yo=.ps.gz) $(addprefix $(outdir)/, $(BIB_FILES:.bib=.html)) $(datafiles)
	$(PYTHON) $(step-bindir)/ls-latex.py --package=$(topdir) --title 'LaTeX documents about LilyPond' \
	  $(YO_FILES) $(OUTYO_FILES:.yo=.latex) $(BIB_FILES) $(DOC_FILES) \
	  | sed "s!$(outdir)/!!g" > $(outdir)/index.html
	  $(PYTHON) $(step-bindir)/add-html-footer.py --package=$(topdir) $(outdir)/index.html

$(outdir)/%.bib: %.bib
	ln -f $< $@

# ignore result since bib2html is nonstandard. Errors would halt the RPM build.j  
$(outdir)/%.html: %.bib
	-bib2html $< $@

# Yeah right: 
# make -k out/mudela.dvi => cp -f out/vocabulary.tex out/mudela.dvi

#out/%: $(outdir)/%
#	cp -f $< $@
