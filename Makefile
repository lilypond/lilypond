include Variables.make 

.SUFFIXES:
.SUFFIXES: .cc .o .hh .y .l .pod .txt .1 .dep

$(exe): $(obs)
	$(CXX) -o $@ $^ $(LOADLIBES)

depend:	; # automatically by gnu make.
clean:
	rm -f $(exe) objects/*.o $(DOCDIR)/* core  
	$(MAKE) -C $(CCDIR) clean
	$(MAKE) -C $(HEADERDIR) clean

distclean: clean
	rm -f  version.hh $(gencc) .GENERATE *~ deps/*.dep

all: kompijl doc

# doc++ documentation of classes
doc:
	$(MAKE) -C Documentation doc

docpp: $(progdocs)
	-mkdir $(DOCDIR)
	doc++ -p -I -d $(DOCDIR) $^

%.o: $(CCDIR)/%.cc
	$(DODEP)\
	$(CXX) -c $(CXXFLAGS) $^  -o $(OBJECTDIR)/$@


$(OBJECTDIR)/%.o: $(CCDIR)/%.cc
	$(DODEP)\
	$(CXX) -c $(CXXFLAGS) $(OUTPUT_OPTION) 


include $(DEPDIR)/*.dep

version.o: $(obs) version.hh

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
