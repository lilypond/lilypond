MAJVER=1
MINVER=0
PATCHLEVEL=14

PACKAGENAME=flower
VERSION=$(MAJVER).$(MINVER).$(PATCHLEVEL)
DNAME=$(PACKAGENAME)-$(VERSION)
DEFINES=-DNDEBUG -O2
#DEFINES=-g
CXXFLAGS+=$(DEFINES)  -Wall -W -pedantic
CXXVER=$(CXX) --version
include Sources.make

obs=$(cc:.cc=.o)
staticlib=libflower.a

DFILES=$(hh) $(cc) $(inl) $(templatecc) Makefile Variables.make make_version\
	Sources.make TODO README
DDIR=$(DNAME)
