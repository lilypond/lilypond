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

INSTALLATION_DIR=$(TEXDIR)/lilypond
INSTALLATION_FILES=$(TEXFILES)

include $(depth)/make/Installfiles.make
