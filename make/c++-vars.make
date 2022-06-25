ALL_LDFLAGS = $(LDFLAGS) $(CONFIG_LDFLAGS) $(MODULE_LDFLAGS) $(CONFIG_LDFLAGS)

EXTRA_CXXFLAGS = \
	-std=c++14 \
	-fno-exceptions \
	-Wall \
	-Wconversion \
	-Wextra \
	-Wold-style-cast \
	-Woverloaded-virtual \
	-Wsuggest-override \
# end

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

ALL_CC_SOURCES += $(HH_FILES) $(INL_FILES) $(CC_FILES) $(YY_FILES) $(LL_FILES)

O_FILES+=$(addprefix $(outdir)/, $(CC_FILES:.cc=.o) $(LL_FILES:.ll=.o) $(YY_FILES:.yy=.o))

TAGS_SOURCES += $(TCC_FILES) $(INL_FILES) $(CC_FILES) $(YY_FILES) $(LL_FILES)
TAGS_HEADERS += $(HH_FILES) $(INL_FILES)

