# tex/Makefile

depth = ..


TEX_FILES = $(wildcard *.tex)

EXTRA_DIST_FILES =  $(TEX_FILES) lilyponddefs.ps

INSTALLATION_DIR=$(datadir)/tex/
INSTALLATION_FILES=$(TEX_FILES)

include $(depth)/make/stepmake.make
include $(stepdir)/installfiles.make

