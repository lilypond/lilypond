# title	   makefile for lilypond
# file	   lily/Makefile 

depth = ..

NAME = lilypond
SUBDIRS = include

EXTRA_DIST_FILES = VERSION 
MODULE_LIBS=$(depth)/lib $(depth)/flower

MAKEFILES=C++ Executable 

include $(depth)/make/Stepmake.make 

# force these: Make can't know these have to be generated in advance
my-lily-lexer.cc: $(outdir)/parser.hh
$(outdir)/lexer.o: $(outdir)/parser.hh


$(outdir)/lily-version.o: $(outdir)/version.hh

$(outdir)/main.o: $(outdir)/BLURB.hh $(outdir)/COPERTINA.hh $(outdir)/FLAPTEKST.hh


# ugh . -> $(outdir)
VERSION: $(depth)/VERSION
	cp $< $@

