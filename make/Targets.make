#
# project  LilyPond -- the musical typesetter
# title	   generic make targets
# file	   make/Targets.make
#
# Copyright (c) 1997 by    
#   	Jan Nieuwenhuizen <jan@digicash.com>
#	Han-Wen Nienhuys <hanwen@stack.nl>

.PHONY : all clean config default dist doc doc++ dummy exe help lib TAGS

# target all:
#
all:	 default
ifdef SUBDIRS
	set -e; for i in $(SUBDIRS); do $(MAKE) -C $$i all; done
endif

#

# platform specific variables,
#
include ./$(depth)/make/out/Site.make
#

# where to do this ?
.PRECIOUS:  $(makeout)/Site.make

# dependency list of executable:
#
EXECUTABLE = $(lily_bindir)/$(NAME)
$(EXECUTABLE): $(build) $(OFILES) $(CUSTOMLIBES) 
	$(INCREASE_BUILD)
	$(MAKE) -S $(OFILES)  $(SILENT_LOG)
#	$(STRIPDEBUG) $(STABLEOBS)
	$(LD_COMMAND) $(OFILES) $(LOADLIBES)

exe: $(EXECUTABLE)
#

$(build): $(depth)/.version
	echo 0 > $@

# dependency list of library:
#
LIBRARY = $(libdir)/$(LIB_PREFIX)$(NAME)$(LIB_SUFFIX)
$(LIBRARY): $(build) $(OFILES) $(CUSTOMLIBES)
	$(INCREASE_BUILD)
	$(MAKE) $(OFILES)  $(SILENT_LOG)
	$(AR_COMMAND) $(OFILES)
	$(RANLIB_COMMAND)


#
lib: $(LIBRARY)
#

clean: localclean
	rm -f $(allobs) $(alldeps)
ifdef SUBDIRS
	set -e; for i in $(SUBDIRS); do $(MAKE) -C $$i clean; done
endif

distclean: clean
	rm -rf Makefile $(lily-version) $(flower-version) $(mi2mu-version) .b $(build) *~ $(allout) $(allgen)


# configure:
#
config:
	./$(depth)/configure
#

# dummydeps:
#
dummydep: $(DUMMYDEPS)
#

# value of $(OSTYPE) on windhoos; "make $OSTYPE" if you use bash :-)
#
win32: 
	$(MAKE) -C . CXX=g++ 
#

# xcompile to doze:
#
doze:	dos
dos: 
	$(MAKE) -C . CXX="gcc-go32 -I/usr/i386-go32/include -I/usr/i386-go32/include/g++ -D_WIN32 -Dcaddr_t=char* -DMAP_SHARED=0"
#

# target help:
#
help:
	@echo "Usage:"
	@echo "	$(MAKE) ["VARIABLE=value" ...] [target]"
	@echo
	@echo "targets:"
	@echo "	all clean config dist distclean doc doc++"
	@echo "	exe help lib moduledist TAGS"
	@echo "	dos:	xcomplile to dos"
	@echo "	win32:	native cygnus-win32 compile" 
#

doc:
	$(MAKE) -C $(depth)/Documentation do-doc

# doc++ documentation of classes
doc++: $(progdocs)	
	doc++ -kp -d $(DOCDIR) $^

dist:
	-mkdir $(distdir)
	$(MAKE) localdist
	chmod -Rf a+rX $(distdir)

	(cd ./$(depth); tar cfz $(DIST_NAME).tar.gz $(DIST_NAME))
	rm -rf $(distdir)/  # should be trapped

localdist: $(DISTFILES)
	if [ -d out ]; then mkdir $(distdir)/$(localdir)/out; fi
	ln $(DISTFILES) $(distdir)/$(localdir)
ifdef SUBDIRS
	set -e; for i in $(SUBDIRS); do mkdir $(distdir)/$(localdir)/$$i; \
		$(MAKE) localdir=$(localdir)/$$i -C $$i localdist; done
endif

moduledist:
	-mkdir $(module-distdir)
	$(MAKE) localmoduledist
	(cd ./$(depth); tar cfz $(MODULE_DIST_NAME).tar.gz $(MODULE_DIST_NAME))
	rm -rf $(module-distdir)/  # should be trapped

localmoduledist:
	ln $(DISTFILES) $(module-distdir)/$(localdir)
ifdef SUBDIRS
	set -e; for i in $(SUBDIRS); do mkdir $(module-distdir)/$(localdir)/$$i; done
	set -e; for i in $(SUBDIRS); do $(MAKE) localdir=$(localdir)/$$i -C $$i localmoduledist; done
endif

TAGS:$(all-tag-sources)
ifdef all-tag-sources
	-etags -CT $(all-tag-sources) /dev/null
endif
ifdef SUBDIRS
	set -e; for i in $(SUBDIRS); do $(MAKE) -C $$i TAGS ; done
endif


# version stuff:
#

$(outdir)/version.hh: .version
	./$(lily_bindir)/make_version > $@


# should this be in Rules?
configure: configure.in
	autoconf - < $<> $@
	chmod +x configure

localclean:


install: localinstall
ifdef SUBDIRS
	set -e; for i in $(SUBDIRS); do $(MAKE) -C $$i install; done
endif

localinstall:

uninstall: localuninstall
ifdef SUBDIRS
	set -e; for i in $(SUBDIRS); do $(MAKE) -C $$i uninstall; done
endif

localuninstall:

# specific stuff:
#
$(LIBFLOWER): check-flower-deps

check-flower-deps:
	$(MAKE)  -C $(depth)/flower/ $(outdir)/$(notdir $(LIBFLOWER))

check-lily-deps: check-flower-deps
	$(MAKE)  -C $(depth)/lib

check-doc-deps:
	$(MAKE) -C $(depth)/Documentation

$(LIBLILY): dummy
	$(MAKE) ./$(outdir)/$(@F) -C $(depth)/lib
#

