# title	   makefile for lilypond
# file	   lily/Makefile 

depth = ..

NAME = lilypond
SUBDIRS = include

EXTRA_DIST_FILES = VERSION 
MODULE_LIBS=$(depth)/lib $(depth)/flower

include $(depth)/make/Stepmake.make 
include $(stepdir)/C++.make
include $(stepdir)/Executable.make

include Stable.make
include VERSION

# force these: Make can't know these have to be generated in advance
my-lily-lexer.cc: $(outdir)/parser.hh
$(outdir)/lexer.o: $(outdir)/parser.hh


$(outdir)/lily-version.o: $(outdir)/version.hh

$(outdir)/main.o: $(outdir)/BLURB.hh $(outdir)/COPERTINA.hh $(outdir)/FLAPTEKST.hh


VERSION: $(depth)/VERSION
	cp $< $@
