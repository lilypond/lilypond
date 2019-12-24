ARFLAGS = ru

ALL_LDFLAGS = $(LDFLAGS) $(CONFIG_LDFLAGS) $(MODULE_LDFLAGS) $(CONFIG_LDFLAGS)

ifeq ($(MINGW_BUILD),)
ifeq ($(CYGWIN_BUILD),)
PIC_FLAGS = -fpic -fPIC
endif
endif

o-dep-out = $(outdir)/$(subst .o,.dep,$(notdir $@))#
DO_O_DEP = rm -f $(o-dep-out); DEPENDENCIES_OUTPUT="$(o-dep-out) $(outdir)/$(notdir $@)"

lo-dep-out = $(outdir)/$(subst .lo,.dep,$(notdir $@))#
DO_LO_DEP = rm -f $(lo-dep-out); DEPENDENCIES_OUTPUT="$(lo-dep-out) $(outdir)/$(notdir $@)"

EXTRA_CXXFLAGS = -std=c++11 -W -Wall -Wconversion -Woverloaded-virtual
#ifeq ($(MY_PATCH_LEVEL),)
#EXTRA_CXXFLAGS += -Werror
#endif

ALL_CXXPPFLAGS = $(CPPFLAGS) $(CONFIG_CPPFLAGS) $(DEFINES) $(INCLUDES:%=-I%)
# note: CXXFLAGS last allows user override of prior flags
ALL_CXXFLAGS = $(ALL_CXXPPFLAGS) $(CONFIG_CXXFLAGS) $(MODULE_CXXFLAGS) $(EXTRA_CXXFLAGS) $(CXXFLAGS)

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

