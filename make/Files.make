# project  LilyPond -- the musical typesetter
# title	   automatic file variables
# file	   make/Files.make
# abstract mmm, brr.
#          
#
# Copyright (c) 1997 by    
#   	Jan Nieuwenhuizen <jan@digicash.com>
#	Han-Wen Nienhuys <hanwen@stack.nl>

# list of c++ header files:
# 
HHFILES = $(wildcard *.hh)
#

# list of c++ inline files:
# 
INLFILES = $(wildcard *.icc)
#

# list of c++ template files:
# 
TCCFILES = $(wildcard *.tcc)
#

# list plain c++ source files:
# 
CCFILES = $(wildcard *.cc)
#

# list of other source files:
#
EXTRA_SOURCE_FILES = $(wildcard *.y *.l)
#

PODFILES = $(wildcard *.pod)

MAKEFILES = $(wildcard *.make)

ALL_SOURCES=$(HHFILES) $(CCFILES) $(EXTRA_SOURCE_FILES) $(INLFILES) \
	$(TCCFILES) $(PODFILES) $(MAKEFILES)

DEPFILES = $(wildcard $(depdir)/*.dep)

build = $(outdir)/.build

all-tag-sources=$(CCFILES) $(HHFILES)
