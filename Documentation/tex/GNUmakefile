# Documentation/tex/Makefile

depth=../..

DATA_FILES = $(wildcard *.data)
datafiles = $(addprefix $(outdir)/,$(DATA_FILES:.data=.html))
TEX_FILES = $(wildcard *.tex)
DOC_FILES = $(wildcard *.doc)

DVI_FILES = $(addprefix $(outdir)/,$(DOC_FILES:.doc=.dvi)  $(TELY_FILES:.tely=.dvi))

#OUTDOC_FILES = $(addprefix $(outdir)/, $(DOC_FILES))
#OUTTEX_FILES = $(addprefix $(outdir)/, $(TEX_FILES))

EXTRA_DIST_FILES= $(DOC_FILES) $(DATA_FILES) $(EL_FILES) $(TEX_FILES) testje.fly
HTML_FILES = $(addprefix $(outdir)/, $(TELY_FILES:.tely=.html))

PS_FILES = $(DVI_FILES:.dvi=.ps)  $(OUTDOC_FILES:.doc=.ps) $(OUTTEX_FILES:.tex=.ps)
PS_GZ_FILES= $(addsuffix .gz, $(PS_FILES))

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

$(outdir)/%.latex: %.data $(depth)/VERSION
	$(PYTHON) $(step-bindir)/table-to-html.py --columns=7 --linesep=''  -o $@ --format=latex $<





local-WWW: $(HTML_FILES)   $(datafiles) $(PS_GZ_FILES)
	$(PYTHON) $(step-bindir)/ls-latex.py --title 'User documentation about LilyPond' \
	   $(DOC_FILES) $(TEX_FILES) $(TELY_FILES) \
	  | sed "s!$(outdir)/!!g" > $(outdir)/index.html
	$(footify) $(outdir)/index.html

$(outdir)/%.bib: %.bib
	ln -f $< $@

localclean:
	rm -f fonts.aux fonts.log feta*.tfm feta*.*pk
