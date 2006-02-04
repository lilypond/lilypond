# -*-Makefile-*-
# title	 specific top level makefile for StepMake

# subdir level:
#
depth = ..

# descent order into subdirectories:
#
SUBDIRS = bin stepmake

# list of distribution files:
#
SCRIPTS =  aclocal.m4 autogen.sh
README_FILES = CHANGES README TODO
README_TXT_FILES =
EXTRA_DIST_FILES = $(README_FILES)  $(SCRIPTS)
NON_ESSENTIAL_DIST_FILES = $(README_TXT_FILES)
#

# bootstrap stepmake:
#
STEPMAKE_TEMPLATES= texinfo
include $(depth)/make/stepmake.make

# don't install stepmake
install: local-install
	@echo Not installing stepmake

localclean:
	rm -f bin/*.pyc
