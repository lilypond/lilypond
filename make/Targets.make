#
# project  LilyPond -- the musical typesetter
# title	   generic make targets
# file	   make/Targets.make
#
# Copyright (c) 1997 by    
#   	Jan Nieuwenhuizen <jan@digicash.com>
#	Han-Wen Nienhuys <hanwen@stack.nl>

.PHONY : all clean config default dist doc doc++ dummy exe help html lib TAGS\
	check-flower-deps check-lib-deps check-doc-deps check-html-deps

# target all:
#
all:	 default
	$(LOOP)
# dependency list of executable:
#

$(EXECUTABLE): $(configheader) $(OFILES) $(outdir)/version.hh
ifdef MODULE_LIBDEPS
	$(MAKE) $(MODULE_LIBDEPS)
endif
ifdef STABLEOBS
	$(DO_STRIP) $(STABLEOBS)
endif
	$(LD_COMMAND) $(OFILES) $(LOADLIBES)

exe: $(EXECUTABLE)


# dependency list of library:
#
$(LIBRARY): $(configheader) $(OFILES)
	$(AR_COMMAND) $(OFILES)
	$(AR) ts $@		#silly irix
	$(RANLIB_COMMAND)

$(SHAREDLIBRARY):  $(configheader) $(OFILES) $(MODULE_LIBDEPS)
	$(LD_COMMAND) $(OFILES) -o $@.$(VERSION)
	rm -f $@
	ln -sf $(outdir)/$(LIB_PREFIX)$(NAME).so.$(VERSION) $@.$(MAJOR_VERSION)
	ln -sf $(LIB_PREFIX)$(NAME).so.$(VERSION) $@

#
lib: $(LIBRARY)
#

outdirs: outdir
	$(LOOP)

outdir:
	-mkdir -p $(outdir)

# be careful about deletion.
clean: localclean
	-rm -f $(outdir)/*
	touch $(outdir)/dummy.dep
	$(LOOP)

distclean: clean 
	$(LOOP)
	$(MAKE) local-distclean

maintainerclean: 
	$(LOOP)
	$(MAKE)	local-maintainerclean
	$(MAKE) local-distclean



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
	@echo "Makefile for LilyPond $(TOPLEVEL_VERSION)"
	@echo "Usage:"
	@echo "	$(MAKE) ["VARIABLE=value" ...] [target]"
	@echo
	@echo "targets:"
	@echo "	all clean config dist distclean doc doc++"
	@echo "	exe help lib TAGS"
	@echo "	dos:	xcomplile to dos"
	@echo "	windows32: native cygnus-gnu compile" 
#

doc:
#	$(MAKE) -C $(depth)/Documentation do-doc
	$(MAKE) -C $(depth)/Documentation all

html: $(HTMLFILES)

# ugh. should generate in out/
dist:
	rm -rf $(distdir)
	-mkdir $(distdir)
	$(MAKE) localdist
	chmod -R a+r $(distdir)
	chmod  a+x `find $(distdir) -type d -print`

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
	ln $(lilyout)/lilypond $(distdir)/bin/lilypond.exe
	strip -s $(distdir)/bin/lilypond.exe
	ln $(mi2muout)/mi2mu $(distdir)/bin/mi2mu.exe
	strip -s $(distdir)/bin/mi2mu.exe
	(cd ./$(depth); $(ZIP) $(outdir)/$(DIST_NAME).exe.zip $(distdir))
# should be trapped
	rm -rf $(distdir)/


localdist: $(DISTFILES) $(OUT_DISTFILES)
	touch $(outdir)/dummy.dep; \
	mkdir $(distdir)/$(localdir)/out; \
	$(LN) $(DISTFILES) $(distdir)/$(localdir)
	$(LN) $(outdir)/dummy.dep $(OUT_DISTFILES) $(distdir)/$(localdir)/out
ifdef SUBDIRS
	set -e; for i in $(SUBDIRS); do mkdir $(distdir)/$(localdir)/$$i; \
		$(MAKE) localdir=$(localdir)/$$i -C $$i localdist; done
endif


TAGS:$(all-tag-sources)
ifdef all-tag-sources
	-etags -CT $(all-tag-sources) $(ERROR_LOG)
	-ctags -CT $(all-tag-sources) $(ERROR_LOG)
endif
	$(LOOP)


# version stuff:
#

$(outdir)/version.hh: VERSION
	sh ./$(lily_bindir)/make-version.sh > $@


# should this be in Rules?
configure: configure.in aclocal.m4
	autoconf - < $<> $@
	chmod +x configure

localclean:

local-distclean:

local-maintainerclean:

install-strip:
	$(MAKE) INSTALL="$(INSTALL) -s" install

install: localinstall
	$(LOOP)

localinstall:

uninstall: localuninstall
	$(LOOP)

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

check-html-deps:
	$(MAKE) -C $(depth)/Documentation html

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

$(configheader): $(depth)/$(configuration).hh
	cp $< $@

WWW: local-WWW
	$(LOOP)

ifneq ($(DEPFILES),)
include $(DEPFILES)
endif
