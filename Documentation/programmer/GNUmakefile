# Documentation/tex/Makefile

depth=../..

TEX_FILES = $(wildcard *.tex)
DOC_FILES = $(wildcard *.doc)
DVI_FILES = $(addprefix $(outdir)/,$(DOC_FILES:.doc=.dvi)  $(TELY_FILES:.tely=.dvi))

OUTTEX_FILES = $(addprefix $(outdir)/, $(TEX_FILES))
OUTDOC_FILES = $(addprefix $(outdir)/, $(DOC_FILES))

EXTRA_DIST_FILES= $(DOC_FILES) $(TEX_FILES) $(wildcard *.sty) 
HTML_FILES = $(addprefix $(outdir)/,  $(TEXI_FILES:.texi=.html) $(TELY_FILES:.tely=.html))
PS_FILES = $(DVI_FILES:.dvi=.ps)

STEPMAKE_TEMPLATES=tex documentation texinfo
LOCALSTEPMAKE_TEMPLATES=lilypond mudela

export BIBINPUTS:=$(shell pwd)//$(PATHSEP)$(BIBINPUTS)
include $(depth)/make/stepmake.make 

dvi: $(DVI_FILES)

ps: $(PS_FILES)

# urg
default:


localclean:
	rm -f fonts.aux fonts.log feta*.tfm feta*.*pk

local-WWW: $(HTML_FILES) $(addsuffix .gz, $(PS_FILES))
	$(PYTHON) $(step-bindir)/ls-latex.py --title 'Hacker documentation' \
	   $(DOC_FILES) $(TEXI_FILES) $(TELY_FILES) \
	  | sed "s!$(outdir)/!!g" > $(outdir)/index.html
