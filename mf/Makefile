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
lyout = $(depth)/init
texout = $(depth)/tex
LYTABLES = $(addprefix $(lyout)/, $(FONT_FILES:.mf=.ly))
TEXTABLES = $(addprefix $(texout)/, $(FONT_FILES:.mf=.tex))
#

all: $(LYTABLES) $(TEXTABLES)

localclean:
	rm -f $(LYTABLES) $(TEXTABLES)

$(outdir)/%.log: %.mf
	mf $<
	mv $(@F) $@
	rm $(shell basename $< .mf).*gf

$(lyout)/%.ly $(texout)/%.tex: $(outdir)/%.log
	mf-to-table -l $(lyout)/$(<F:.log=.ly) -t $(texout)/$(<F:.log=.tex) $<
	
localinstall:
	$(INSTALL) -d $(MFDIR)/lilypond
	$(INSTALL) -m 644 $(MFFILES) $(MFDIR)/lilypond/

localuninstall:
	for i in $(MFFILES) ; do rm -f $(MFDIR)/lilypond/$$i; done
	-rmdir $(MFDIR)/lilypond/

# dependencies:
#
$(lyout)/dyn10.ly: autometric.mf ital-f.mf ital-m.mf ital-p.mf ital-r.mf ital-s.mf ital-z.mf dyn10.mf
$(lyout)/font-en-tja16.ly: autometric.mf bolletjes.mf eindelijk.mf font-en-tja16.mf wat-zie-ik.mf
$(lyout)/font-en-tja20.ly: autometric.mf bolletjes.mf eindelijk.mf font-en-tja20.mf wat-zie-ik.mf
#
$(texout)/dyn10.tex: autometric.mf ital-f.mf ital-m.mf ital-p.mf ital-r.mf ital-s.mf ital-z.mf dyn10.mf
$(texout)/font-en-tja16.tex: autometric.mf bolletjes.mf eindelijk.mf font-en-tja16.mf wat-zie-ik.mf
$(texout)/font-en-tja20.tex: autometric.mf bolletjes.mf eindelijk.mf font-en-tja20.mf wat-zie-ik.mf
#


