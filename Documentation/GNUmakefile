# Documentation/Makefile

depth = ..

NAME = documentation
SUBDIRS=man tex pictures topdocs ntweb
STEPMAKE_TEMPLATES=documentation 
README_TOP_FILES=NEWS DEDICATION TODO ANNOUNCE-0.1 AIMS
INFO_FILES = $(wildcard $(outdir)/$(package).info*)



BLURBS=BLURB COPERTINA FLAPTEKST

include $(depth)/make/stepmake.make 

default: do-doc

info: $(outdir)/$(package).info

