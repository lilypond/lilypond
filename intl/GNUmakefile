# intl/GNUmakefile

depth = ..
NAME = intl

SED_FILES = $(wildcard *.sed)

# should be set by configure...
URG_DEFINES = -DGNULOCALEDIR=\"$(localedir)\" -DLOCALE_ALIAS_PATH=\"$(localedir):\"
EXTRA_CFLAGS = -I. $(URG_DEFINES)

EXTRA_DIST_FILES = Makefile.in.dist cat-compat.c.dist ChangeLog libintl.inst $(SED_FILES)

STEPMAKE_TEMPLATES=c library

$(outdir)/library.a:
	@echo "*** To install libintl type 'make lib; make install-lib' ***"

include $(depth)/make/stepmake.make

lib:  $(LIBRARY)
LIBINTL = $(LIBRARY)

# localinstall: $(LIBINTL)
install-lib: $(LIBINTL)
	$(INSTALL) -d $(includedir)
#	$(INSTALL) libintl.h $(includedir)
	$(INSTALL) libintl.inst $(includedir)/libintl.h
ifeq ($(LIB_SUFFIX),.so)
	$(INSTALL) -d $(libdir)
	$(INSTALL) $(LIBINTL).$(VERSION) $(libdir)
	ln -s $(LIB_PREFIX)intl$(LIB_SUFFIX).$(VERSION) $(libdir)/$(LIB_PREFIX)intl$(LIB_SUFFIX).$(MAJOR_VERSION)
	ln -s $(LIB_PREFIX)intl$(LIB_SUFFIX).$(VERSION) $(libdir)/$(LIB_PREFIX)intl$(LIB_SUFFIX)
else
	$(INSTALL) -d $(libdir)
	$(INSTALL) $(LIBINTL) $(libdir)/$(INSTALL_LIBRARY)
endif

localuninstall:
	rm -f $(includedir)/libintl.h
	rm -f $(libdir)/libintl.{so*,a}

