
# added two warnings that are treated by cygwin32's gcc 2.7.2 as errors.
# huh, but still, no warnings even provoced with linux's gcc 2.7.2.1?

# -pipe makes it go faster, but is not supported on all platforms. 
# EXTRA_CXXFLAGS= -Wall -Winline -W -Wmissing-prototypes -Wmissing-declarations -Wconversion
EXTRA_CXXFLAGS= -Wall -W -Wmissing-prototypes -Wmissing-declarations -Wconversion


 CXXFLAGS = $(ICFLAGS) $(DEFINES) $(addprefix -I,$(INCLUDES)) $(USER_CFLAGS) $(EXTRA_CFLAGS) $(MODULE_CFLAGS) $($(PACKAGE)_CFLAGS) $($(PACKAGE)_CXXFLAGS) $(USER_CXXFLAGS) $(EXTRA_CXXFLAGS) $(MODULE_CXXFLAGS)
CXX_OUTPUT_OPTION = $< -o $@
DO_CXX_COMPILE=$(DODEP) $(CXX) -c $(CXXFLAGS) $(CXX_OUTPUT_OPTION)

# template files:
TCC_FILES := $(wildcard *.tcc)
HH_FILES := $(wildcard *.hh)
CC_FILES := $(wildcard *.cc)
INL_FILES := $(wildcard *.icc)
YY_FILES := $(wildcard *.yy)
LL_FILES := $(wildcard *.ll)

SOURCE_FILES+=$(YY_FILES) $(CC_FILES) $(INL_FILES) $(TCC_FILES) $(HH_FILES) $(LL_FILES)
O_FILES+=$(addprefix $(outdir)/, $(YY_FILES:.yy=.o) $(CC_FILES:.cc=.o) $(LL_FILES:.ll=.o))

TAGS_FILES += $(TCC_FILES) $(HH_FILES) $(INL_FILES) $(CC_FILES)

# hmm
configheader=$(outdir)/config.hh
