# tex/Makefile

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
DISTFILES = Makefile $(TEXFILES)
#

localinstall:
	$(INSTALL) -d $(TEXDIR)/lilypond
	$(INSTALL) -m 755 $(TEXFILES) $(TEXDIR)/lilypond/

localuninstall:
	for i in $(TEXFILES) ; do rm -f $(TEXDIR)/lilypond/$$i; done
	-rmdir $(TEXDIR)/lilypond/
