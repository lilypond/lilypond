
depth=../..
include ./$(depth)/make/Include.make 
include ./$(depth)/make/Docrules.make
#

DOCFILES = $(wildcard *.doc)
OUTDOCFILES = $(addprefix $(outdir)/, $(DOCFILES))
BIBFILES= $(wildcard *.bib)
EXTRA_DISTFILES= $(BIBFILES) $(DOCFILES)

dvi: $(DVIFILES)

local-WWW: $(OUTDOCFILES:.doc=.ps.gz) $(addprefix $(outdir)/, $(BIBFILES:.bib=.html))
	ls-latex --title 'LaTeX documents about LilyPond' $(BIBFILES) $(DOCFILES) > $(outdir)/index.html

$(outdir)/%.bib: %.bib
	ln -f $< $@

$(outdir)/%.html: %.bib
	bib2html $< $@
