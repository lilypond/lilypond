# init/Makefile

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
INIFILES = $(shell ls *.ini)
DISTFILES = Makefile $(INIFILES)
#

localinstall:
	$(INSTALL) -d $(datadir)/lilypond/init
	$(INSTALL) -m 755 $(INIFILES) $(datadir)/lilypond/init

localuninstall:
	for i in $(INIFILES) ; do rm -f $(datadir)/lilypond/init/$$i; done
	-rmdir $(datadir)/lilypond/init $(datadir)/lilypond/
