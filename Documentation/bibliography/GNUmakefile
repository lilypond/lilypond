# Documentation/tex/Makefile

depth=../..

OUT_BIB_FILES = $(addprefix $(outdir)/, $(BIB_FILES))
BIB_FILES= $(wildcard *.bib)
EXTRA_DIST_FILES= $(BIB_FILES)

STEPMAKE_TEMPLATES=tex documentation
LOCALSTEPMAKE_TEMPLATES=lilypond ly

export BIBINPUTS:=$(shell pwd)//$(PATHSEP)$(BIBINPUTS)
include $(depth)/make/stepmake.make 

dvi: $(DVI_FILES) $(OUT_BIB_FILES)

ps: $(PS_FILES)

# urg
default:

GENHTMLS = engraving colorado  computer-notation
OUTGENHTMLS = $(addprefix $(outdir)/, $(GENHTMLS:%=%.html))

local-WWW: $(addprefix $(outdir)/, $(BIB_FILES:.bib=.html))
	$(PYTHON) $(step-bindir)/ls-latex.py  --title 'References on Music Notation' \
	  $(YO_FILES) $(OUTYO_FILES:.yo=.latex) $(BIB_FILES) $(DOC_FILES) $(TEX_FILES) \
	  | sed "s!$(outdir)/!!g" > $(outdir)/index.html

$(outdir)/%.bib: %.bib
	ln -f $< $@
# we want footers even if website builds (or is built) partly
	$(MAKE) footify


# ignore result since bib2html is nonstandard. Errors would halt the RPM build.j  
$(outdir)/%.html: %.bib
	-bib2html $< $@
	$(footify) $@

localclean:
	rm -f fonts.aux fonts.log feta*.tfm feta*.*pk
