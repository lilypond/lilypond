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
	$(INSTALL) -d $(TEXPREFIX)/lilypond
	$(INSTALL) -m 755 $(TEXFILES) $(TEXPREFIX)/lilypond/

localuninstall:
	for i in $(TEXFILES) ; do rm -f $(TEXPREFIX)/lilypond/$$i; done
	-rmdir $(TEXPREFIX)/lilypond/
