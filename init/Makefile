# init/Makefile

depth = ..

# generic stuff/Makefile
#
include ./$(depth)/make/Include.make

INIFILES = $(wildcard *.ly)
DISTFILES = Makefile $(INIFILES)

INSTALLATION_DIR=$(datadir)/lilypond/
INSTALLATION_FILES=$(INIFILES)

include $(depth)/make/Installfiles.make

