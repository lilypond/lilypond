# title	   makefile for mi2mu
# file	   mi2mu/Makefile 

depth = ..

NAME = mi2mu
MODULE_NAME = mi2mu

SUBDIRS = include
EXTRA_DIST_FILES += TODO
MODULE_LIBS=$(depth)/lib $(depth)/flower
STEPMAKE_TEMPLATES=c++ executable

include $(depth)/make/stepmake.make 

# explicit dependencies: (how to do auto?)
#
midi-lexer.l:	$(outdir)/midi-parser.hh


$(outdir)/mi2mu-version.o: $(outdir)/version.hh

