# Documentation/topdocs/Makefile

depth = ../..

SUBDIRS=
BLURBS=BLURB #COPERTINA FLAPTEKST
AT_FILES = $(BLURBS) # 
at-dir = $(doc-dir)/
at-ext = .in
BLURBS=BLURB COPERTINA FLAPTEKST
OUT_DIST_FILES=$(OUTTXT_FILES)

STEPMAKE_TEMPLATES=documentation yolily-topdoc install install-out

include $(depth)/make/stepmake.make 

INSTALLATION_DIR=$(datadir)/Documentation/topdocs
INSTALLATION_FILES=$(DIST_FILES)
POST_INSTALL=-mkdir $(INSTALLATION_DIR)/out

INSTALLATION_OUT_DIR=$(datadir)/Documentation/topdocs/out
INSTALLATION_OUT_FILES=$(OUTTXT_FILES)

