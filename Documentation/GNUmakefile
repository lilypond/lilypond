# Documentation/Makefile

depth = ..

NAME = documentation
SUBDIRS= tex metadoc bibliography pictures topdocs ntweb
STEPMAKE_TEMPLATES=documentation  texinfo

README_TOP_FILES=NEWS DEDICATION TODO  AIMS
INFO_FILES = $(wildcard $(outdir)/$(package).info*)
EXTRA_DIST_FILES = COPYRIGHT 



BLURBS=BLURB 

include $(depth)/make/stepmake.make 

default: do-doc

info: $(outdir)/$(package).info

local-WWW: copy-for-me

copy-for-me:
	$(foreach a, $(README_TOP_FILES),cp ../$(a) $(outdir)/$(a).txt && ) true
