# tex/Makefile

# subdir level:
#
depth = ..
#

build = ./$(depth)/lily/$(outdir)/.build

# generic stuff/Makefile
#
include ./$(depth)/make/Stuff.make
#

# list of distribution files:
# 
TEXFILES = $(shell ls *.tex)
DISTFILES = Makefile $(TEXFILES)
#

localinstall:
	$(INSTALL) -d $(TEXPREFIX)/lilypond
	$(INSTALL) -m 755 $(TEXFILES) $(TEXPREFIX)/lilypond/

localuninstall:
	for i in $(TEXFILES) ; do rm -f $(TEXPREFIX)/lilypond/$$i; done
	-rmdir $(TEXPREFIX)/lilypond/