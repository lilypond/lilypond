# title	   top level makefile for FlowerLib
# file	   flower/Makefile

# should reinstate versioning if shared libs are enabled.

depth = ..

NAME = flower
MODULE_NAME = flower
SUBDIRS = include 

SCRIPTS = 
README_FILES = NEWS-1.0 NEWS-1.1.46 README TODO
EXTRA_DIST_FILES= VERSION $(README_FILES) $(SCRIPTS)


include $(depth)/make/Stepmake.make 
include $(stepdir)/C++.make
include $(stepdir)/Library.make

