# Documentation/Makefile

depth = ..

NAME = Documentation
SUBDIRS=man tex pictures topdocs
MAKEFILES=Documentation
README_TOP_FILES=NEWS DEDICATION TODO ANNOUNCE-0.1

EXTRA_DIST_FILES = gnu-music-history TODO 

BLURBS=BLURB COPERTINA FLAPTEKST
OUTTEXINFO_FILES = $(outdir)/topinfo.texinfo $(outdir)/mutopia.texinfo

include $(depth)/make/Stepmake.make 

default: do-doc

# INFO_FILES = $(outdir)/$(package).info

texinfo: $(OUTTEXINFO_FILES)

