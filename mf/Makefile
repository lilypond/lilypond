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
EXTRA_DISTFILES = $(MFFILES) $(TEXFILES) TODO
#

FONT_FILES = $(wildcard *[0-9].mf)
FET_FILES = $(wildcard font-en-tja*[0-9].mf)

lyout = $(depth)/init
texout = $(depth)/tex
LYTABLES = $(addprefix $(lyout)/, $(FET_FILES:.mf=.ly))
TEXTABLES = $(addprefix $(texout)/, $(FET_FILES:.mf=.tex))
#

all: $(LYTABLES) $(TEXTABLES)

localclean:
	rm -f $(LYTABLES) $(TEXTABLES) $(outdir)/mfdeps

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
	$(depth)/bin/$(outdir)/mf-to-table --ly $(lyout)/$(<F:.log=.ly) --tex $(texout)/$(<F:.log=.tex) $<

localinstall:
	$(INSTALL) -d $(MFDIR)/lilypond
	$(INSTALL) -m 644 $(MFFILES) $(MFDIR)/lilypond/

localuninstall:
	for i in $(MFFILES) ; do rm -f $(MFDIR)/lilypond/$$i; done
	-rmdir $(MFDIR)/lilypond/

$(outdir)/mfdeps: $(FONT_FILES)
# do something silly to avoid barfs if python not installed.
	echo > $@
	$(depth)/bin/$(outdir)/mf-deps  $^ >> $@

# dependencies:
#
include $(outdir)/mfdeps
#




