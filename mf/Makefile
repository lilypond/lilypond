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
MFDEPS = $(outdir)/mf.dep
EXTRA_DISTFILES = $(MFFILES) $(TEXFILES) TODO README
#

FONT_FILES = $(wildcard *[0-9].mf)
FET_FILES = $(wildcard feta[0-9]*.mf)

lyout = $(depth)/init
texout = $(depth)/tex
LYTABLES = $(addprefix $(lyout)/, $(FET_FILES:.mf=.ly))
TEXTABLES = $(addprefix $(texout)/, $(FET_FILES:.mf=.tex))
#

all: $(LYTABLES) $(TEXTABLES)

localclean:
	rm -f $(LYTABLES) $(TEXTABLES) 

localrealclean: 
	rm -f $(MFDEPS)

$(outdir)/%.dvi: %.mf
	mf $<
	gftodvi  $(basename $<)
	mv   $(basename $<).dvi $(outdir)
	rm $(basename $<).*gf

$(outdir)/%.log: %.mf
	mf $<
	mv $(@F) $@
	rm $(basename $< ).*gf

$(lyout)/%.ly $(texout)/%.tex: $(outdir)/%.log
	$(PYTHON) $(depth)/bin/mf-to-table.py --ly $(lyout)/$(<F:.log=.ly) --tex $(texout)/$(<F:.log=.tex) $<

localinstall:
	$(INSTALL) -d $(MFDIR)/public/lilypond
	$(INSTALL) -m 644 $(MFFILES) $(MFDIR)/public/lilypond/

localuninstall:
	for i in $(MFFILES) ; do rm -f $(MFDIR)/lilypond/$$i; done
	-rmdir $(MFDIR)/public/lilypond/

$(MFDEPS): $(FONT_FILES)
# do something silly to avoid barfs if python not installed.
	echo > $@
	$(PYTHON) $(depth)/bin/mf-deps.py  $^ >> $@

# dependencies:
#
include $(MFDEPS)
#

