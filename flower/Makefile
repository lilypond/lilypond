include Variables.make

$(staticlib): $(obs) fversion.hh
	$(AR) cr libflower.a $(obs)

include depend

depend: Sources.make
	$(CXX) -MM $(cc) > depend


clean:
	rm -f $(obs) $(staticlib)

realclean: clean
	rm -f depend

fversion.hh: Variables.make make_version
	make_version $(MAJVER) $(MINVER) $(PATCHLEVEL) "$(CXX) `$(CXXVER)`" > $@


dist:
	-mkdir $(DDIR)
	ln $(DFILES) $(DDIR)/
	tar cfz $(DNAME).tar.gz $(DDIR)/*
	rm -rf $(DDIR)/

TAGS:
	etags -CT $(inl) $(cc) $(hh)

