# title	   makefile for lilypond
# file	   lily/Makefile 

depth = ..

NAME = lilypond
SUBDIRS = include

MODULE_LIBS=$(depth)/lib $(depth)/flower

STEPMAKE_TEMPLATES= c++ executable 

include $(depth)/make/stepmake.make 

# force these: Make can't know these have to be generated in advance
$(outdir)/my-lily-parser.o: $(outdir)/parser.hh
$(outdir)/my-lily-lexer.o: $(outdir)/parser.hh
$(outdir)/lexer.o: $(outdir)/parser.hh
$(outdir)/lily-version.o: $(outdir)/version.hh
$(outdir)/main.o: $(outdir)/BLURB.hh $(outdir)/COPERTINA.hh $(outdir)/FLAPTEKST.hh



