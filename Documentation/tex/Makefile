
depth=../..
include ./$(depth)/make/Include.make 
include ./$(depth)/make/Docrules.make
#

DOCFILES = $(wildcard *.doc)
OUTDOCFILES = $(addprefix $(outdir)/, $(DOCFILES))
BIBFILES= $(wildcard *.bib)
EXTRA_DISTFILES= $(BIBFILES) $(DOCFILES)

dvi: $(DVIFILES)

local-WWW: $(OUTDOCFILES:.doc=.ps.gz)
	ls-latex --extension .ps.gz $(DOCFILES) > $(outdir)/index.html
