# Documentation/Makefile

depth = ..

NAME = Documentation
SUBDIRS=man tex pictures topdocs

include $(depth)/make/Stepmake.make 

default: do-doc

README_TOP_FILES=NEWS DEDICATION TODO ANNOUNCE-0.1

EXTRA_DIST_FILES = gnu-music-history TODO 

BLURBS=BLURB COPERTINA FLAPTEKST
# INFO_FILES = $(outdir)/$(package).info

OUTTEXINFO_FILES = $(outdir)/topinfo.texinfo $(outdir)/mutopia.texinfo

texinfo: $(OUTTEXINFO_FILES)

include $(stepdir)/Documentation.shared.make
