# title	   makefile for midi2ly
# file	   midi2ly/Makefile 

depth = ..

NAME = midi2ly
MODULE_NAME = midi2ly

SUBDIRS = include
EXTRA_DIST_FILES += TODO
MODULE_LIBS=$(depth)/flower
HELP2MAN_EXECS = midi2ly
STEPMAKE_TEMPLATES=c++ executable po help2man

include $(depth)/make/stepmake.make 

# explicit dependencies: (how to do auto?)
#
midi-lexer.l:	$(outdir)/midi-parser.hh


$(outdir)/midi2ly-version.o: $(outdir)/version.hh

