MAJVER=1
MINVER=0
PATCHLEVEL=4

PACKAGENAME=flower
VERSION=$(MAJVER).$(MINVER).$(PATCHLEVEL)
DNAME=$(PACKAGENAME)-$(VERSION)
DEFINES=-DNDEBUG -DNPRINT -O2
#DEFINES=-g
CXXFLAGS+=$(DEFINES)  -Wall -W -pedantic

include Sources.make

obs=$(cc:.cc=.o)
staticlib=libflower.a
$(staticlib): $(obs)
	$(AR) cr libflower.a $(obs)

include depend

depend: Sources.make
	$(CXX) -MM $(cc) > depend


clean:
	rm -f $(obs) $(staticlib)
realclean: clean
	rm -f depend
DFILES=$(hh) $(cc) $(inl) $(templatecc) Makefile Sources.make TODO
DDIR=$(DNAME)

dist:
	-mkdir $(DDIR)
	ln $(DFILES) $(DDIR)/
	tar cfz $(DNAME).tar.gz $(DDIR)/*
	rm -rf $(DDIR)/
TAGS:
	etags -CT $(inl) $(cc) $(hh)

