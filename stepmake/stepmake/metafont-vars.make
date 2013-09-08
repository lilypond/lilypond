MF_FILES := $(call src-wildcard,*.mf)
MF_TFM_FILES = $(addprefix $(outdir)/, $(FONT_FILES:.mf=.tfm))
MF_DVI_FILES = $(addprefix $(outdir)/, $(FONT_FILES:.mf=.dvi))
MF_LOG_FILES = $(addprefix $(outdir)/, $(FONT_FILES:.mf=.log))
DVI_FILES += $(MF_DVI_FILES)
TFM_FILES += $(MF_TFM_FILES)

MF2PT1_OPTIONS=--rounding=0.0001 \
               --family=$(notdir $(<:%.mf=%)) \
               --fullname=$(notdir $(<:%.mf=%)) \
               --name=$(notdir $(<:%.mf=%))

ifdef QUIET_BUILD
METAFONT_QUIET = >/dev/null
else
METAFONT_QUIET =
endif

# Find the metafont file $(1) within the source dirs and return its path.
# If not found, return $(outdir)/$(1) assuming that it is a generated file.
find-mf = \
$(firstword \
	$(wildcard $(src-dir)/$(1)) \
	$(wildcard $(top-src-dir)/mf/$(1)) \
	$(outdir)/$(1) \
)

# Recursively scan the metafont .mf file $(1) for "input X;"
# and return all dependencies.
scan-mf = \
$(foreach f, $(shell test -f $(1) && sed -ne "/^[[:space:]]*input[[:space:]]/s/^[[:space:]]*input\([^.;]*\)\(.mf;\|;\)/\1.mf/p" $(1)), \
	$(call find-mf,$(f)) \
	$(call scan-mf,$(call find-mf,$(f))) \
)

# Find dependencies for the target $@, based on the metafont source file $<,
# and write the dependencies to a .dep file.
DO_MF_DEP = ( echo ./$@: $(call scan-mf,$<) > $(basename $@).dep ) &&
