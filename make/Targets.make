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
	for i in $(SUBDIRS); do $(MAKE) -C $$i all; done
#

# platform specific variables,
#
include ./$(depth)/make/out/Site.make
#

# where to do this ?
.PRECIOUS:  $(makeout)/Site.make

# ... and configure bootstrap :-)
#
$(makeout)/Site.make: $(make-dir)/$(genout) $(flower-config) $(lily-config)
# this is handy, but runs on second "make distclean" too. ah well...
#	if [ \! -d $(makeout) ]; then mkdir $(makeout); fi
	touch $@
	@echo "oeps, sources were not configured!"
	(cd $(depth); ./configure)
#

# dependency list of executable:
#
EXECUTABLE = $(bindir)/$(NAME)
$(EXECUTABLE): $(OFILES) $(CUSTOMLIBES)
#	$(STRIPDEBUG) $(STABLEOBS)
#	$(LD_COMMAND) -o $@ $^ $(LOADLIBES)
	$(LD_COMMAND) $(OFILES) $(LOADLIBES)
	-@touch $(VERSION_DEPENDENCY) $(ERROR_LOG)
	$(INCREASE_BUILD)
	touch $(build) #waai necessary?
#
exe: $(EXECUTABLE)
#

# dependency list of library:
#
LIBRARY = $(libdir)/$(LIB_PREFIX)$(NAME)$(LIB_SUFFIX)
$(LIBRARY): $(OFILES) $(CUSTOMLIBES)
	$(AR_COMMAND) $(OFILES)
	-@touch $(VERSION_DEPENDENCY) $(ERROR_LOG)
	$(INCREASE_BUILD)
	touch $(build) #waai necessary?
#
lib: $(LIBRARY)
#

clean:
	rm -f $(allexe) core $(allobs) $(alldeps)
	for i in $(SUBDIRS); do $(MAKE) -C $$i clean; done

distclean: clean
	rm -rf  $(lily-version) $(flower-version) $(mi2mu-version) .b $(build) *~ $(allout) $(allgen)


# configure:
#
config:
	./$(depth)/configure
#

# dummydeps:
#
dummydep: $(flower-dir)/$(genout) $(lib-dir)/$(genout) $(lily-dir)/$(genout) $(mi2mu-dir)/$(genout) $(DUMMYDEPS)
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
	$(MAKE) -C Documentation do-doc

# doc++ documentation of classes
doc++: $(progdocs)	
	doc++ -kp -d $(DOCDIR) $^

dist:
	-mkdir $(distdir)
	$(MAKE) localdist
	(cd ./$(depth); tar cfz $(DIST_NAME).tar.gz $(DIST_NAME))
	rm -rf $(distdir)/  # should be trapped

localdist:
	ln $(DISTFILES) $(distdir)/$(localdir)
	for i in $(SUBDIRS); do mkdir $(distdir)/$(localdir)/$$i; done
	for i in $(SUBDIRS); do $(MAKE) localdir=$(localdir)/$$i -C $$i localdist; done

moduledist:
	-mkdir $(module-distdir)
	$(MAKE) localmoduledist
	(cd ./$(depth); tar cfz $(MODULE_DIST_NAME).tar.gz $(MODULE_DIST_NAME))
	rm -rf $(module-distdir)/  # should be trapped

localmoduledist:
	ln $(DISTFILES) $(module-distdir)/$(localdir)
	for i in $(SUBDIRS); do mkdir $(module-distdir)/$(localdir)/$$i; done
	for i in $(SUBDIRS); do $(MAKE) localdir=$(localdir)/$$i -C $$i localmoduledist; done

all-tags: TAGS
	for i in $(SUBDIRS); do $(MAKE) -C $$i all-tags; done

TAGS: $(allcc)
	etags -CT $(allcc) 

# to some outdir?
autoconf:
	autoconf -  < configure.in > ac_configure  


# version stuff:
#
check-flower-version:
	$(MAKE) flower-version -C ./$(depth)/flower
$(lily-version): $(lily-dir)/$(genout) ./$(depth)/.version ./$(bindir)/make_version $(build)
	./$(bindir)/make_version "$(MAJOR_VERSION)" "$(MINOR_VERSION)" "$(PATCH_LEVEL)" "$(MY_PATCH_LEVEL)" "$(BUILD)" "$(CXX) $(CXXVER)" > $@
check-mi2mu-version:
	$(MAKE) mi2mu-version -C ./$(depth)/mi2mu
#

