ifndef LIB_SUFFIX
LIB_SUFFIX = .a
endif

LIB_PREFIX = lib

# librarian:
#
AR = ar
AR_COMMAND = $(AR) $(ARFLAGS) $@

#
ifeq ($(RANLIB),error)
RANLIB_COMMAND=$(AR) ts $@
else
RANLIB_COMMAND=$(RANLIB) $@
endif
# linker:
LIBRARY = $(outdir)/library.a
SHAREDLIBRARY=$(outdir)/$(LIB_PREFIX)$(NAME).so
