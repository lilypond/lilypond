
#
# template files:
TCC_FILES := $(wildcard *.tcc)
HH_FILES := $(wildcard *.hh)
CC_FILES := $(wildcard *.cc)
INL_FILES := $(wildcard *.icc)
YY_FILES := $(wildcard *.yy)
LL_FILES := $(wildcard *.ll)

SOURCE_FILES+=$(YY_FILES) $(CC_FILES) $(INL_FILES) $(TCC_FILES) $(HH_FILES) $(LL_FILES)
OBJECT_FILES+=$(YY_FILES:.yy=.o) $(CC_FILES:.cc=.o) $(LL_FILES:.ll=.o)

TAGS_FILES += $(TCC_FILES) $(HH_FILES) $(INL_FILES) $(CC_FILES)
