
ifneq ($(CYGWIN_BUILD),)
  SHARED_LIB_PREFIX = cyg
  SHARED_LIB_SUFFIX = 
  SHARED_LIB_VERSION = $(subst .,-,$(VERSION))
  SHARED_LIB_VERSION_SUFFIX = .dll
  ALL_LDFLAGS += -Wl,--image-base=0x10000000 -Wl,--out-implib,lib$(NAME).a
else
  SHARED_LIB_PREFIX = lib
  SHARED_LIB_SUFFIX = .so
  SHARED_LIB_VERSION = $(VERSION)
  SHARED_LIB_VERSION_SUFFIX =
endif

INSTALL_SHARED_LIBRARY = $(SHARED_LIB_PREFIX)$(NAME)$(SHARED_LIB_SUFFIX)$(SHARED_LIB_VERSION)$(SHARED_LIB_VERSION_SUFFIX)
SHARED_LIBRARY = $(outdir)/$(INSTALL_SHARED_LIBRARY)


lo-dep-out = $(outdir)/$(subst .lo,.dep,$(notdir $@))#
DO_LO_DEP = rm -f $(lo-dep-out); DEPENDENCIES_OUTPUT="$(lo-dep-out) $(outdir)/$(notdir $@)"

LO_FILES += $(addprefix $(outdir)/, $(Y_FILES:.y=.lo) $(C_FILES:.c=.lo) $(L_FILES:.l=.lo))

