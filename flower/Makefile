PACKAGENAME=flower
VERSION=1.0
DNAME=$(PACKAGENAME)-$(VERSION)
CXXFLAGS+=-g -Wall

cc=lgetopt.cc 	 string.cc dataf.cc textdb.cc unionfind.cc 

templatecc=cursor.cc list.cc 
inl=findcurs.inl link.inl list.inl
hh=cursor.hh cursor.inl lgetopt.hh link.hh list.hh \
	string.hh stringutil.hh vray.hh textdb.hh textstr.hh  assoc.hh\
	findcurs.hh unionfind.hh compare.hh



obs=$(cc:.cc=.o)
staticlib=libflower.a
$(staticlib): $(obs)
	ar cr libflower.a $(obs)

include depend

depend: Makefile
	$(CXX) -MM $(cc) > depend


clean:
	rm depend $(obs) $(staticlib)

DFILES=$(hh) $(cc) $(inl) $(templatecc) Makefile
DDIR=$(DNAME)

dist:
	-mkdir $(DDIR)
	ln $(DFILES) $(DDIR)/
	tar cfz $(DNAME).tar.gz $(DDIR)/*
	rm -rf $(DDIR)/

	
