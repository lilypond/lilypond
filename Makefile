# Makefile
# top level makefile of LilyPond

include Variables.make 

.SUFFIXES:
.SUFFIXES: .cc .o .hh .y .l .pod .txt .1 .dep


$(exe): $(obs)
	$(STRIPDEBUG) $(STABLEOBS)
	$(LINKER) -o $@ $^ $(LOADLIBES)

$(m2m):	$(m2mobs)
	$(LINKER) -o $@ $^ $(LOADLIBES)

.PHONY: clean docxx

clean:
	rm -f $(allexe) $(DOCDIR)/* core $(allobs) 
	for SUBDIR in $(SUBDIRS); \
	do \
		$(MAKE) SUBDIR=$$SUBDIR -C $$SUBDIR clean;\
	done

distclean: clean
	rm -f  version.hh $(gencc) .GENERATE *~ $(ALLDEPS) 

all: $(exe) $(m2m) doc

# value of $(OSTYPE) on windhoos; "make $OSTYPE" if you use bash :-)
win32: 
	$(MAKE) -C . CXX=g++ 

doc:
	$(MAKE) -C Documentation doc

# doc++ documentation of classes
docxx: $(progdocs)	
	doc++ -kp -d $(DOCDIR) $^


include $(DEPDIR)/*.dep

$(OBJECTDIR)/%.o: $(CCDIR)/%.cc
	$(DODEP)\
	$(CXX) -c $(CXXFLAGS) $(OUTPUT_OPTION) 

$(OBJECTDIR)/version.o: $(obs) $(HEADERDIR)/version.hh

include Generate.make

dist:
	-mkdir $(DDIR)
	ln $(DFILES) $(DDIR)/
	for SUBDIR in $(SUBDIRS); \
	do	mkdir $(DDIR)/$$SUBDIR; \
		$(MAKE) SUBDIR=$$SUBDIR -C $$SUBDIR dist;\
	done
	tar cfz $(DNAME).tar.gz $(DNAME)/
	rm -rf $(DDIR)/

TAGS:
	$(MAKE) -C $(HEADERDIR) TAGS
	$(MAKE) -C $(CCDIR) TAGS
