#
# project  LilyPond -- the musical typesetter
# title	   generic make rules
# file	   make/Rules.make
#
# Copyright (c) 1997 by    
#   	Jan Nieuwenhuizen <jan@digicash.com>
#	Han-Wen Nienhuys <hanwen@stack.nl>

.PHONY : all clean default dist doc doc++ dummy exe help lib tags

# target all:
#
all:	 default
	for i in $(SUBDIRS); do $(MAKE) -C $$i all; done
#

# dependency list of executable:
#
EXECUTABLE = $(bindir)/$(NAME)
$(EXECUTABLE): $(OFILES) $(CUSTOMLIBES)
#	$(STRIPDEBUG) $(STABLEOBS)
#	$(LD_COMMAND) -o $@ $^ $(LOADLIBES)
	$(LD_COMMAND) $(OFILES) $(LOADLIBES)
	touch $(VERSION_DEPENDENCY)
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
	touch $(VERSION_DEPENDENCY)
	$(INCREASE_BUILD)
	touch $(build) #waai necessary?
#
lib: $(LIBRARY)
#

clean:
	rm -f $(allexe) core $(allobs) 
	for i in $(SUBDIRS); do $(MAKE) -C $$i clean; done

distclean: clean
	rm -rf  $(lily-version) $(flower-version) .b $(build) .GENERATE *~ $(allout)


# configure:
#
config:
	$(bindir)/configure
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
	@echo "	exe help lib moduledist tags"
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

all-tags: tags
	for i in $(SUBDIRS); do $(MAKE) -C $$i all-tags; done

tags:
	etags -CT $(allcc) 

# version stuff:
#
check-flower-version:
	$(MAKE) flower-version -C ./$(depth)/flower
$(lily-version): ./$(depth)/.version ./$(bindir)/make_version $(build)
	./$(bindir)/make_version "$(MAJOR_VERSION)" "$(MINOR_VERSION)" "$(PATCH_LEVEL)" "$(MY_PATCH_LEVEL)" "$(BUILD)" "$(CXX) $(CXXVER)" > $@
#

