# Documentation/tex/Makefile

depth=../..

DATA_FILES = $(wildcard *.data)
datafiles = $(addprefix $(outdir)/,$(DATA_FILES:.data=.html))
TEX_FILES = $(wildcard *.tex)
DOC_FILES = $(wildcard *.doc)

DVI_FILES = $(addprefix $(outdir)/,$(DOC_FILES:.doc=.dvi)  $(TELY_FILES:.tely=.dvi))
OUT_BIB_FILES = $(addprefix $(outdir)/, $(BIB_FILES))


OUTTEX_FILES = $(addprefix $(outdir)/, $(TEX_FILES))
OUTDOC_FILES = $(addprefix $(outdir)/, $(DOC_FILES))
EL_FILES = $(wildcard *.el)
BIB_FILES= $(wildcard *.bib)
EXTRA_DIST_FILES= $(BIB_FILES) $(DOC_FILES) $(DATA_FILES) $(EL_FILES)  $(TEX_FILES) $(wildcard *.sty) 
HTML_FILES = $(addprefix $(outdir)/,  $(TELY_FILES:.tely=.html))
PS_FILES = $(DVI_FILES:.dvi=.ps)

STEPMAKE_TEMPLATES=tex documentation
LOCALSTEPMAKE_TEMPLATES=lilypond mudela

export BIBINPUTS:=$(shell pwd)//$(PATHSEP)$(BIBINPUTS)
include $(depth)/make/stepmake.make 






dvi: $(DVI_FILES)

ps: $(PS_FILES)

# urg
default:

GENHTMLS = engraving colorado  computer-notation
OUTGENHTMLS = $(addprefix $(outdir)/, $(GENHTMLS:%=%.html))

#urg should generalise and move Lilypond -> StepMake
# URG.  Lilypond specific. Move out. 
$(outdir)/%.html: %.data $(depth)/VERSION
	$(PYTHON) $(step-bindir)/table-to-html.py --columns=7 --linesep='' --package=$(topdir) -o $@ $<

$(outdir)/%.tex: %.data $(depth)/VERSION
	$(PYTHON) $(step-bindir)/table-to-html.py --columns=7 --linesep='' --package=$(topdir) -o $@ --latex $<



local-WWW-donotdoit: $(HTML_FILES) $(OUTTEX_FILES:.doc=.ps.gz)  $(addprefix $(outdir)/, $(BIB_FILES:.bib=.html)) $(datafiles) ps
	$(PYTHON) $(step-bindir)/ls-latex.py --title 'LaTeX documents about design and implementation of LilyPond' \
	  $(TELY_FILES)  $(BIB_FILES) $(DOC_FILES) $(TEX_FILES)\
	  | sed "s!$(outdir)/!!g" > $(outdir)/index.html

$(outdir)/%.bib: %.bib
	ln -f $< $@

# ignore result since bib2html is nonstandard. Errors would halt the RPM build.j  
$(outdir)/%.html: %.bib
	-bib2html $< $@

# Yeah right: 
# make -k out/mudela.dvi => cp -f out/vocabulary.tex out/mudela.dvi

#out/%: $(outdir)/%
#	cp -f $< $@

locamlclean:
	rm -f fonts.aux fonts.log feta*.tfm feta*.*pk
