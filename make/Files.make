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

EXTRA_SOURCE_FILES = $(wildcard *.yy *.ll)

#
PODFILES = $(wildcard *.pod)
OUTPODFILES = $(addprefix $(outdir)/,$(PODFILES))


CCFILES = $(wildcard *.cc)
MAKEFILES = $(wildcard *.make)

ALL_SOURCES=$(HHFILES) $(CCFILES) $(EXTRA_SOURCE_FILES) $(INLFILES) \
	$(TCCFILES) $(PODFILES) $(MAKEFILES)

DEPFILES = $(wildcard $(depdir)/*.dep)


all-tag-sources=$(CCFILES) $(HHFILES) $(TCCFILES)
