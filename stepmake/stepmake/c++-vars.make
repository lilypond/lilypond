
include $(stepdir)/compile-vars.make

EXTRA_CXXFLAGS = -W -Wall -Wconversion
#ifeq ($(MY_PATCH_LEVEL),)
#EXTRA_CXXFLAGS += -Werror
#endif

ALL_CXXPPFLAGS = $(CPPFLAGS) $(CONFIG_CPPFLAGS) $(DEFINES) $(INCLUDES:%=-I%)
ALL_CXXFLAGS = $(CXXFLAGS) $(ALL_CXXPPFLAGS) $($(PACKAGE)_CXXFLAGS) $(CONFIG_CXXFLAGS) $(MODULE_CXXFLAGS) $(EXTRA_CXXFLAGS)

TCC_FILES := $(call src-wildcard,*.tcc)
HH_FILES := $(call src-wildcard,*.hh)
CC_FILES := $(call src-wildcard,*.cc)
INL_FILES := $(call src-wildcard,*.icc)
YY_FILES := $(call src-wildcard,*.yy)
LL_FILES := $(call src-wildcard,*.ll)

SOURCE_FILES+= $(CC_FILES) $(YY_FILES) $(INL_FILES) $(TCC_FILES) $(HH_FILES) $(LL_FILES)

ALL_CC_SOURCES += $(HH_FILES) $(INL_FILES) $(CC_FILES) $(YY_FILES) $(LL_FILES)

O_FILES+=$(addprefix $(outdir)/, $(CC_FILES:.cc=.o) $(LL_FILES:.ll=.o) $(YY_FILES:.yy=.o))

TAGS_SOURCES += $(TCC_FILES) $(INL_FILES) $(CC_FILES) $(YY_FILES) $(LL_FILES)
TAGS_HEADERS += $(HH_FILES) $(INL_FILES)

