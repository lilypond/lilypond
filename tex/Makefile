# tex/Makefile

depth = ..

include $(depth)/make/Stepmake.make

TEX_FILES = $(wildcard *.tex)
DIST_FILES = Makefile $(TEX_FILES)

INSTALLATION_DIR=$(TEXDIR)/lilypond
INSTALLATION_FILES=$(TEX_FILES)

include $(stepdir)/Installfiles.make

