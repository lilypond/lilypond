
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
# urg, i don't like this name, it's not what you'd expect
LIBRARY = $(outdir)/library.a

INSTALL_LIBRARY = $(LIB_PREFIX)$(NAME)$(LIB_SUFFIX)
SHARED_LIBRARY=$(outdir)/$(LIB_PREFIX)$(NAME).so
