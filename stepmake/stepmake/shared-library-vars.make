SHARED_LIB_PREFIX = lib
SHARED_LIB_SUFFIX = .so

SHARED_LIBRARY = $(outdir)/$(SHARED_LIB_PREFIX)$(NAME)$(SHARED_LIB_SUFFIX)
INSTALL_SHARED_LIBRARY = $(SHARED_LIB_PREFIX)$(NAME)$(SHARED_LIB_SUFFIX)

lo-dep-out = $(outdir)/$(subst .lo,.dep,$(notdir $@))#
DO_LO_DEP = rm -f $(lo-dep-out); DEPENDENCIES_OUTPUT="$(lo-dep-out) $(outdir)/$(notdir $@)"

LO_FILES += $(addprefix $(outdir)/, $(Y_FILES:.y=.lo) $(C_FILES:.c=.lo) $(L_FILES:.l=.lo))

