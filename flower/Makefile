include Variables.make

$(staticlib): $(obs) fversion.hh
	$(AR) cr libflower.a $(obs)

include depend

depend: Sources.make
	$(CXX) $(CXXFLAGS) -MM $(cc) > depend

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
	tar cfz $(DNAME).tar.gz $(DDIR)/*
	rm -rf $(DDIR)/

TAGS: $(ALLSOURCES)
	etags -CT $(inl) $(cc) $(hh)

