# Automatically generated from ./mf/Makefile.am.wild by autowild
# Do not edit
# Generated automatically by wild-perl 0.1
# mf/Makefile.am.wild

# subdir level:
#
depth = ..
#
#
# include ./$(depth)/make/Include.make
outdir = .
#

TEXFILES = dimen.tex dyn.tex
MFFILES = feta-autometric.mf feta-banier.mf feta-beams16.mf feta-beams20.mf feta-beams26.mf feta-beugel.mf feta-beum.mf feta-black16.mf feta-bolletjes.mf feta-braces16.mf feta-braces20.mf feta-din10.mf feta-eindelijk.mf feta-generic.mf feta-ital-f.mf feta-ital-m.mf feta-ital-p.mf feta-ital-r.mf feta-ital-s.mf feta-ital-z.mf feta-klef.mf feta-macros.mf feta-nummer-generic.mf feta-nummer.mf feta-nummer10.mf feta-nummer12.mf feta-nummer3.mf feta-nummer4.mf feta-nummer5.mf feta-nummer6.mf feta-nummer7.mf feta-nummer8.mf feta-params.mf feta-puntje.mf feta-schrift.mf feta-slag.mf feta-sleur.mf feta-test16.mf feta-test20.mf feta-toevallig.mf feta-watzieik.mf feta11.mf feta13.mf feta16.mf feta19.mf feta20.mf feta23.mf feta26.mf grayimagen.mf
MFDEPS = $(outdir)/mf.dep
EXTRA_DIST = Makefile.am.wild Rules.make $(MFFILES) $(TEXFILES) TODO README

FONT_FILES = feta-beams16.mf feta-beams20.mf feta-beams26.mf feta-black16.mf feta-braces16.mf feta-braces20.mf feta-din10.mf feta-nummer10.mf feta-nummer12.mf feta-nummer3.mf feta-nummer4.mf feta-nummer5.mf feta-nummer6.mf feta-nummer7.mf feta-nummer8.mf feta-test16.mf feta-test20.mf feta11.mf feta13.mf feta16.mf feta19.mf feta20.mf feta23.mf feta26.mf
FET_FILES = feta11.mf feta13.mf feta16.mf feta19.mf feta20.mf feta23.mf feta26.mf

lyout = $(depth)/init
texout = $(depth)/tex
LYTABLES = $(addprefix $(lyout)/, $(FET_FILES:.mf=.ly))
TEXTABLES = $(addprefix $(texout)/, $(FET_FILES:.mf=.tex))

default: all $(LYTABLES) $(TEXTABLES)

localclean:
	rm -f $(LYTABLES) $(TEXTABLES) 

localrealclean: 
	rm -f $(MFDEPS)

localinstall:
	$(INSTALL) -d $(MFDIR)/public/lilypond
	$(INSTALL) -m 644 $(MFFILES) $(MFDIR)/public/lilypond/

localuninstall:
	for i in $(MFFILES) ; do rm -f $(MFDIR)/lilypond/$$i; done
	-rmdir $(MFDIR)/public/lilypond/

include $(srcdir)/Rules.make

# dependencies:
#
include $(MFDEPS)
#

$(srcdir)/Makefile.am: $(srcdir)/Makefile.am.wild
	$(PERL) $(top_srcdir)/bin/wild-perl < $< > $@