ARFLAGS = ru

ALL_LDFLAGS = $(LDFLAGS) $(CONFIG_LDFLAGS) $(MODULE_LDFLAGS) $(CONFIG_LDFLAGS)

ifeq ($(MINGW_BUILD),)
ifeq ($(CYGWIN_BUILD),)
PIC_FLAGS = -fpic -fPIC
endif
endif

EXTRA_CXXFLAGS = -std=c++11 -fno-exceptions -W -Wall -Wconversion -Woverloaded-virtual

o-dep-out = $(outdir)/$(subst .o,.dep,$(notdir $@))#
EXTRA_CXXFLAGS += -MMD -MP -MF $(o-dep-out) -MT $(outdir)/$(notdir $@)

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

