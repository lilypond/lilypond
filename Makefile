include Variables.make 

$(exe): $(obs)
	$(CXX) -o $@ $^ $(LOADLIBES)

clean:
	rm -f $(exe) *.o $(DOCDIR)/* core  

distclean: clean
	rm -f  depend version.hh $(gencc) .GENERATE *~

all: kompijl doc

# doc++ documentation of classes
doc:
	-mkdir $(DOCDIR)
	doc++ -p -I -d $(DOCDIR) $(progdocs)

depend: Sources.make .GENERATE
	touch depend
	$(MAKE) realdepend


$(OBJECTDIR)/%.o: $(CCDIR)/%.cc
	$(CXX) -c $(CXXFLAGS) $(OUTPUT_OPTION)

# hack to create these sources once, before the dependencies
.GENERATE:
	touch .GENERATE depend
	$(MAKE) version.hh
	$(MAKE) $(gencc)
	rm -f depend

realdepend: $(cc)
	$(CXX) $(CXXFLAGS) -MM $^ |  perl -ne 's#hdr/proto.hh##; s/^(.+)\.o/'$(OBJECTDIR)'\/\1.o/; print;' > depend

include depend

$(CCDIR)/parser.cc: parser.y
	$(BISON) -d $<
	mv $(CCDIR)/parser.tab.h $(HEADERDIR)/parser.hh
	mv $(CCDIR)/parser.tab.c $@

parser.hh: parser.cc

version.o: $(obs) version.hh

hdr/version.hh: Variables.make make_version
	make_version $(MAJVER) $(MINVER) $(PATCHLEVEL)  > $@

$(CCDIR)/lexer.cc: lexer.l
	$(FLEX) -+ -t $< > $@

dist:
	-mkdir $(DDIR)
	ln $(DFILES) $(DDIR)/
	for a in $(SUBDIRS); \
	do	mkdir $(DDIR)/$$a; \
		$(MAKE) -C $$a dist;\
	done
	tar cfz $(DNAME).tar.gz $(DNAME)/
	rm -rf $(DDIR)/



TAGS:
	$(MAKE) -C $(HEADERDIR) TAGS
	$(MAKE) -C $(CCDIR) TAGS
