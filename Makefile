include Variables.make 

.SUFFIXES:
.SUFFIXES: .cc .o .hh .y .l

$(exe): $(obs)
	$(CXX) -o $@ $^ $(LOADLIBES)

clean:
#	$(MAKE) -C objects clean
	rm -f $(exe) objects/*.o $(DOCDIR)/* core  
	$(MAKE) -C $(CCDIR) clean
	$(MAKE) -C $(HEADERDIR) clean

distclean: clean
	rm -f  depend version.hh $(gencc) .GENERATE *~

all: kompijl doc

# doc++ documentation of classes
doc: $(progdocs)
	-mkdir $(DOCDIR)
	doc++ -p -I -d $(DOCDIR) $^

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

parsheadorig=$(CCDIR)/parser.tab.h
parsheadnew=$(HEADERDIR)/parser.hh

#
# take some trouble to avoid overwriting the old y.tab.h
$(CCDIR)/parser.cc: parser.y
	$(BISON) -d $<
	(if diff -q $(parsheadorig) $(parsheadnew); then \
		echo leaving $(parsheadnew);  \
	else \
		mv $(parsheadorig) $(parsheadnew); \
	fi )
	mv $(CCDIR)/parser.tab.c $@

parser.hh: parser.cc

version.o: $(obs) version.hh

hdr/version.hh: Variables.make make_version
	make_version $(MAJVER) $(MINVER) $(PATCHLEVEL) "$(CXX) $(CXXVER)" > $@

$(CCDIR)/lexer.cc: lexer.l
	$(FLEX)  -t $< > $@

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
