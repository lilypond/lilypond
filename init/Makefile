# init/Makefile


# subdir level:
#
depth = ..
#

build = ./$(depth)/lily/$(outdir)/.build

# generic stuff/Makefile
#
include ./$(depth)/make/Include.make
#

# list of distribution files:
# 
INIFILES = $(wildcard *.ini)
DISTFILES = Makefile $(INIFILES)
#

localinstall:
	$(INSTALL) -d $(datadir)/lilypond/init
	$(INSTALL) -m 644 $(INIFILES) $(datadir)/lilypond/init

localuninstall:
	for i in $(INIFILES) ; do rm -f $(datadir)/lilypond/init/$$i; done
	-rmdir $(datadir)/lilypond/init $(datadir)/lilypond/
