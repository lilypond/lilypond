#
# project  LilyPond -- the musical typesetter
# title	   generic make targets
# file	   make/Targets.make
#
# Copyright (c) 1997 by    
#   	Jan Nieuwenhuizen <jan@digicash.com>
#	Han-Wen Nienhuys <hanwen@stack.nl>

.PHONY : all clean config default dist doc doc++ dummy exe help lib TAGS html\
	check-flower-deps check-lib-deps check-doc-deps

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

$(EXECUTABLE): $(build) $(OFILES) $(outdir)/version.hh
	$(MAKE) $(MODULE_LIBDEPS) 
	$(INCREASE_BUILD)
	$(MAKE) -S $(OFILES)  $(SILENT_LOG)
ifdef STABLEOBS
	$(DO_STRIP) $(STABLEOBS)
endif
	$(LD_COMMAND) $(OFILES) $(LOADLIBES)

exe: $(EXECUTABLE)

$(build): $(depth)/VERSION
	echo 0 > $@

# dependency list of library:
#
LIBRARY = $(outdir)/$(LIB_PREFIX)$(NAME).a
$(LIBRARY): $(build) $(OFILES)
	$(INCREASE_BUILD)
	$(MAKE) $(OFILES)  $(SILENT_LOG)
	$(AR_COMMAND) $(OFILES)
	$(RANLIB_COMMAND)

SHAREDLIBRARY=$(outdir)/$(LIB_PREFIX)$(NAME).so
$(SHAREDLIBRARY):  $(build) $(OFILES) $(MODULE_LIBDEPS)
	$(INCREASE_BUILD)
	$(MAKE) $(OFILES)  $(SILENT_LOG)
	$(LD_COMMAND) $(OFILES) -o $@.$(VERSION)
	rm -f $@
	ln -sf $(outdir)/$(LIB_PREFIX)$(NAME).so.$(VERSION) $@.$(MAJOR_VERSION)
	ln -sf $(LIB_PREFIX)$(NAME).so.$(VERSION) $@

#
lib: $(LIBRARY)
#

# be careful about deletion.
clean: localclean
	rm -f core 
ifdef EXECUTABLE
	rm -f $(EXECUTABLE)
endif
ifdef OFILES
	rm -f $(OFILES)
endif
ifdef DEPFILES
	rm -f $(DEPFILES)
endif
ifdef SUBDIRS
	set -e; for i in $(SUBDIRS); do $(MAKE) -C $$i clean; done
endif

# configure:
#
config:
	./$(depth)/configure
#

# dummydeps:
#
dummydep: $(DUMMYDEPS)
#

# value of $(OSTYPE) on windhoos...; "make $OSTYPE" if you use bash :-)
#
win32:  windows32 # win/lose?
#
windows32:
	$(MAKE) -C . "CXX=g++ -D_WINDOWS32"
#

# xcompile to doze:
#
doze:	dos
dos: 
	$(MAKE) -C . CXX="gcc-go32 -I/usr/i386-go32/include -I/usr/i386-go32/include/g++ -D_WINDOWS32 -Dcaddr_t=char* -DMAP_SHARED=0"
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
	@echo "	windows32: native cygnus-gnu compile" 
#

doc:
	$(MAKE) -C $(depth)/Documentation do-doc


# ugh. should generate in out/
dist:
	-mkdir $(distdir)
	$(MAKE) localdist
	chmod -Rf a+rX $(distdir)

	(cd ./$(depth)/$(outdir); $(TAR) cf - $(DIST_NAME) | gzip -9 > $(DIST_NAME).tar.gz)

# should be trapped
	rm -rf $(distdir)/

# ugh. should generate in out/
dozedist: doosdist
doosdist:
	-mkdir $(distdir)
#	nogo, SUBDIRS is handed down to subdir...
#	$(MAKE) SUBDIRS="Documentation init input tex" localdist
	$(MAKE) localdist
	chmod -Rf a+rX $(distdir)
#	ugh, the ugly way, then
	(cd $(distdir); rm -rf $(NO_DOOS_DIST))
	cp $(lilyout)/lilypond.exe $(distdir)
	strip -s $(distdir)/lilypond.exe
	cp $(mi2muout)/mi2mu.exe $(distdir)
	strip -s $(distdir)/mi2mu.exe
	(cd ./$(depth); $(ZIP) $(DIST_NAME).exe.zip $(distdir))
# should be trapped
	rm -rf $(distdir)/


localdist: $(DISTFILES)
	if [ -d out ]; then \
		mkdir $(distdir)/$(localdir)/out; \
		touch $(distdir)/$(localdir)/out/dummy.dep; \
	fi
	$(LN) $(DISTFILES) $(distdir)/$(localdir)
ifdef SUBDIRS
	set -e; for i in $(SUBDIRS); do mkdir $(distdir)/$(localdir)/$$i; \
		$(MAKE) localdir=$(localdir)/$$i -C $$i localdist; done
endif

moduledist:
	-mkdir $(module-distdir)
	$(MAKE) localmoduledist
	(cd $(depth)/$(outdir); $(TAR) cfz $(MODULE_DIST_NAME).tar.gz $(MODULE_DIST_NAME))
	rm -rf $(module-distdir)/ 

localmoduledist:
	$(LN) $(DISTFILES) $(module-distdir)/$(localdir)
ifdef SUBDIRS
	set -e; for i in $(SUBDIRS); do mkdir $(module-distdir)/$(localdir)/$$i; done
	set -e; for i in $(SUBDIRS); do $(MAKE) localdir=$(localdir)/$$i -C $$i localmoduledist; done
endif

TAGS:$(all-tag-sources)
ifdef all-tag-sources
	-etags -CT $(all-tag-sources) $(ERROR_LOG)
	-ctags -CT $(all-tag-sources) $(ERROR_LOG)
endif
ifdef SUBDIRS
	set -e; for i in $(SUBDIRS); do $(MAKE) -C $$i TAGS ; done
endif


# version stuff:
#

$(outdir)/version.hh: VERSION
	sh ./$(lily_bindir)/make-version.sh > $@


# should this be in Rules?
configure: configure.in aclocal.m4
	autoconf - < $<> $@
	chmod +x configure

localclean:

install-strip:
	$(MAKE) INSTALL="$(INSTALL) -s" install

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
	$(MAKE)  -C $(depth)/flower/ default

check-lib-deps: check-flower-deps
	$(MAKE)  -C $(depth)/lib

check-doc-deps:
	$(MAKE) -C $(depth)/Documentation

$(LIBLILY): dummy
	$(MAKE) ./$(outdir)/$(@F) -C $(depth)/lib


# RedHat rpm package:
#
rpm: check-rpm-doc-deps
	-cp $(depth)/lilypond-$(TOPLEVEL_VERSION).tar.gz $(rpm-sources)
#	-cp $(wildcard $(depth)/Documentation/*.xpm) $(rpm-sources)
	-cp $(wildcard $(depth)/Documentation/$(outdir)/*.gif) $(rpm-sources)
	$(MAKE) -C $(make-dir) spec
	rpm -ba $(makeout)/lilypond.spec

check-rpm-doc-deps: 
	$(MAKE) -C $(depth)/Documentation gifs

check-mf-deps:
	$(MAKE) -C $(depth)/mf



installexe:
	$(INSTALL) -d $(bindir)
	$(INSTALL) -m 755 $(EXECUTABLES) $(bindir)

uninstallexe:
	for a in $(EXECUTABLES); do rm -f $(bindir)/`basename $$a`; done

ifneq ($(DEPFILES),)
include $(DEPFILES)
endif
