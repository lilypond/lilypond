
#ugh ugh .
ifndef LIB_SUFFIX
LIB_SUFFIX = .a
endif

LIB_PREFIX = lib

# librarian:
#
AR = ar
AR_COMMAND = $(AR) $(ARFLAGS) $@

# thanks to Nelson Beebe for this trick.
RANLIB_COMMAND=$(RANLIB) $@ || $(AR) ts $@ || true

# linker:
LIBRARY = $(outdir)/library.a
SHAREDLIBRARY=$(outdir)/$(LIB_PREFIX)$(NAME).so
