# mf/Makefile

# subdir level:
#
depth = ..
#
#
include ./$(depth)/make/Include.make
#

# list of distribution files:
# 
TEXFILES = $(wildcard *.tex)
MFFILES = $(wildcard *.mf)
EXTRA_DISTFILES =   TODO README  $(MFFILES)
#
lyout = $(depth)/init
texout = $(depth)/tex

FONT_FILES = $(wildcard *[0-9].mf)
FET_FILES = $(wildcard feta[0-9]*.mf)

LYTABLES = $(addprefix $(lyout)/, $(FET_FILES:.mf=.ly))
LOGFILES = $(addprefix $(outdir)/, $(FET_FILES:.mf=.log))
TEXTABLES = $(addprefix $(texout)/, $(FET_FILES:.mf=.tex))
#

default: $(LYTABLES) $(TEXTABLES) $(LOGFILES)

localclean:
	rm -f $(LYTABLES) $(TEXTABLES)

$(outdir)/%.dvi: %.mf
	mf $<
	gftodvi  $(basename $<)
	mv   $(basename $<).dvi $(outdir)
	rm $(basename $<).*gf

$(outdir)/%.log: %.mf
	mf $<
	mv $(@F) $@
	rm $(basename $< ).*gf

$(lyout)/%.ly $(texout)/%.tex $(outdir)/%.dep: $(outdir)/%.log
	$(PYTHON) $(depth)/bin/mf-to-table.py --outdir=$(outdir) --dep $(outdir)/$(<F:.log=.dep) --ly $(lyout)/$(<F:.log=.ly) --tex $(texout)/$(<F:.log=.tex) $<


INSTALLATION_DIR=$(MFDIR)/public/lilypond
INSTALLATION_FILES=$(MFFILES)

include $(depth)/make/Installfiles.make
