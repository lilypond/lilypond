include Variables.make

$(staticlib): $(obs) fversion.hh
	$(AR) cr libflower.a $(obs)

include deps/*.dep

deps/*.dep:
	touch deps/dummy.dep

depend: Sources.make
	$(CXX) $(CXXFLAGS) -MM $(cc) > depend


$(OBJECTDIR)/%.o: $(CCDIR)/%.cc
	$(DODEP)\
	$(CXX) -c $(CXXFLAGS) $(OUTPUT_OPTION) 


clean:
	rm -f $(obs) $(staticlib) fversion.hh

realclean: clean
	rm -f depend

fversion.hh: Variables.make make_version
	make_version $(MAJVER) $(MINVER) $(PATCHLEVEL) "$(CXX) `$(CXXVER)`" > $@

.PHONY: docxx
docxx: $(hh) $(cc) $(templatecc) $(inl)
	doc++ -p -d docxx $(hh) $(cc) $(templatecc) $(inl)

dist:
	-mkdir $(DDIR)
	ln $(DFILES) $(DDIR)/
	-for SUBDIR in $(SUBDIRS); \
	do	mkdir $(DDIR)/$$SUBDIR; \
		$(MAKE) SUBDIR=$$SUBDIR -C $$SUBDIR dist;\
	done
	tar cfz $(DNAME).tar.gz $(DNAME)/
	rm -rf $(DDIR)/

TAGS: $(ALLSOURCES)
	etags -CT $(inl) $(cc) $(hh)



test: stringtest

stringtest: $(OBJECTDIR)/stringtest.o 
	g++ -o $@ $< -L. -lflower