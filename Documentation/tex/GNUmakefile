# Documentation/tex/Makefile

depth=../..

DATA_FILES = $(wildcard *.data)
datafiles = $(addprefix $(outdir)/,$(DATA_FILES:.data=.html))
TEX_FILES = $(wildcard *.tex)
DOC_FILES = $(wildcard *.doc)

DVI_FILES = $(addprefix $(outdir)/,$(DOC_FILES:.doc=.dvi)  $(TELY_FILES:.tely=.dvi))



OUTDOC_FILES = $(addprefix $(outdir)/, $(DOC_FILES))
OUTTEX_FILES = $(addprefix $(outdir)/, $(TEX_FILES))

EXTRA_DIST_FILES= $(DOC_FILES) $(DATA_FILES) $(EL_FILES) $(TEX_FILES) testje.fly
HTML_FILES = $(addprefix $(outdir)/, $(TELY_FILES:.tely=.html))

PS_FILES = $(DVI_FILES:.dvi=.ps)

STEPMAKE_TEMPLATES=tex texinfo documentation
LOCALSTEPMAKE_TEMPLATES=lilypond mudela

include $(depth)/make/stepmake.make 






dvi: $(DVI_FILES)

ps: $(PS_FILES)

# urg
default:

GENHTMLS = engraving colorado glossary computer-notation
OUTGENHTMLS = $(addprefix $(outdir)/, $(GENHTMLS:%=%.html))

#urg should generalise and move Lilypond -> StepMake
# URG.  Lilypond specific. Move out. 
$(outdir)/%.html: %.data $(depth)/VERSION
	$(PYTHON) $(step-bindir)/table-to-html.py --columns=7 --linesep='' -o $@ $<
	$(PYTHON) $(step-bindir)/add-html-footer.py --package=$(topdir) $@

$(outdir)/%.latex: %.data $(depth)/VERSION
	$(PYTHON) $(step-bindir)/table-to-html.py --columns=7 --linesep=''  -o $@ --format=latex $<





local-WWW: $(HTML_FILES) $(OUTDOC_FILES:.doc=.ps.gz) $(OUTTEX_FILES:.tex=.ps.gz)   $(datafiles)
	$(PYTHON) $(step-bindir)/ls-latex.py --package=$(topdir) --title 'LaTeX documents about LilyPond' \
	   $(DOC_FILES) $(TEX_FILES)\
	  | sed "s!$(outdir)/!!g" > $(outdir)/index.html
	  $(PYTHON) $(step-bindir)/add-html-footer.py --package=$(topdir) $(outdir)/index.html

$(outdir)/%.bib: %.bib
	ln -f $< $@

localclean:
	rm -f fonts.aux fonts.log feta*.tfm feta*.*pk
