# title	   makefile for stepmake

# subdir level:
#
depth = ../..
#

# descent order into subdirectories:
#
SUBDIRS =
#

STEPMAKE_TEMPLATES=makedir install

# bootstrap stepmake:
#
-include $(depth)/make/stepmake.make

ifeq ($(PACKAGE),STEPMAKE)
INSTALLATION_DIR=$(datadir)/stepmake
else
INSTALLATION_DIR=$(dir $(patsubst %/, %, $(dir $(datadir))))$(package)/stepmake/stepmake
endif
INSTALLATION_FILES=GNUmakefile $(MAKE_FILES)

