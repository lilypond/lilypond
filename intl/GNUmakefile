# intl/GNUmakefile

depth = ..
NAME = intl

SED_FILES = $(wildcard *.sed)

# should be set by configure...
URG_DEFINES = -DGNULOCALEDIR=\"$(localedir)\" -DLOCALE_ALIAS_PATH=\"$(localedir):\"
EXTRA_CFLAGS = -I. $(URG_DEFINES)

EXTRA_DIST_FILES = Makefile.in.dist cat-compat.c.dist ChangeLog $(SED_FILES)

ifeq ($(strip $(LIBINTL)),)
STEPMAKE_TEMPLATES=c
else
STEPMAKE_TEMPLATES=c library install-library
endif

# catch default targets: don't make, install etc.
default: $(outdir)
	@echo "*** To install libintl type 'make lib; make install-lib' ***"

$(outdir)/library.a:
	@echo "*** To install libintl type 'make lib; make install-lib' ***"

local-install: $(outdir)
	@echo "*** To install libintl type 'make lib; make install-lib' ***"

local-uninstall: $(outdir)
	@echo "*** To uninstall libintl type 'make uninstall-lib' ***"

include $(depth)/make/stepmake.make


# deferred targets
lib:
	$(MAKE) LIBINTL=yes all

install-lib: $(LIBRARY)
	$(MAKE) LIBINTL=yes local-install

uninstall-lib:
	$(MAKE) LIBINTL=yes local-uninstall

