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
tableout = $(depth)/init
TABLES = $(addprefix $(tableout)/, $(FONT_FILES:.mf=.ly))
#

all: $(TABLES)

# localclean:
# 	rm -f $(TABLES)

$(outdir)/%.log: %.mf
	mf $<
	mv $(@F) $@
	rm $(shell basename $< .mf).*gf

$(tableout)/%.ly: $(outdir)/%.log
	mf-to-table -o $@ $<
	
localinstall:
	$(INSTALL) -d $(MFDIR)/lilypond
	$(INSTALL) -m 644 $(MFFILES) $(MFDIR)/lilypond/

localuninstall:
	for i in $(MFFILES) ; do rm -f $(MFDIR)/lilypond/$$i; done
	-rmdir $(MFDIR)/lilypond/

# dependencies:
#
$(tableout)/dyn10.ly: autometric.mf ital-f.mf ital-m.mf ital-p.mf ital-r.mf ital-s.mf ital-z.mf dyn10.mf
$(tableout)/font-en-tja16.ly: autometric.mf bolletjes.mf font-en-tja16.mf
$(tableout)/font-en-tja20.ly: autometric.mf bolletjes.mf font-en-tja20.mf
#


