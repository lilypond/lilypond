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
DISTFILES = Makefile $(MFFILES) $(TEXFILES)
#

localinstall:
	$(INSTALL) -d $(MFDIR)/lilypond
	$(INSTALL) -m 644 $(MFFILES) $(MFDIR)/lilypond/

localuninstall:
	for i in $(MFFILES) ; do rm -f $(MFDIR)/lilypond/$$i; done
	-rmdir $(MFDIR)/lilypond/
