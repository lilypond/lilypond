# file	   make/Makefile 

depth = ..
STEPMAKE_TEMPLATES=makedir install
BLURBS=BLURB # COPERTINA FLAPTEKST

ifneq ($(strip $(state-vector)),)
EXTRA_DIST_FILES = STATE-VECTOR
endif

include $(depth)/make/stepmake.make 

INSTALLATION_DIR=$(datadir)/make
INSTALLATION_FILES=$(DIST_FILES)
