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
# list of other source files:
#

EXTRA_SOURCE_FILES = $(wildcard *.y *.l)

#
PODFILES = $(wildcard *.pod)

CCFILES = $(wildcard *.cc)
MAKEFILES = $(wildcard *.make)

ALL_SOURCES=$(HHFILES) $(CCFILES) $(EXTRA_SOURCE_FILES) $(INLFILES) \
	$(TCCFILES) $(PODFILES) $(MAKEFILES)

DEPFILES = $(wildcard $(depdir)/*.dep)

build = $(outdir)/.build

all-tag-sources=$(CCFILES) $(HHFILES)
