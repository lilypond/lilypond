MAJVER=1
MINVER=0
PATCHLEVEL=17
PACKAGENAME=flower

#PROFILEFLAG=-pg
DEBUGFLAG=-g -O2
OPTIFLAG=-DNDEBUG -DNPRINT -O2

#########################################

ifdef PROFILEFLAG
	DEFINES+=$(OPTIFLAG) $(PROFILEFLAG)
	EXTRALIB+=-pg
endif

ifndef DEBUGFLAG
	DEFINES+=$(OPTIFLAG)
else
	DEFINES+=$(DEBUGFLAG)
endif


CXXFLAGS+=$(DEFINES)  -Wall -W 
CXXVER=$(CXX) --version

VERSION=$(MAJVER).$(MINVER).$(PATCHLEVEL)
DNAME=$(PACKAGENAME)-$(VERSION)

include Sources.make

obs=$(cc:.cc=.o)
staticlib=libflower.a

DFILES=$(hh) $(cc) $(inl) $(templatecc) Makefile Variables.make make_version\
	Sources.make TODO README
DDIR=$(DNAME)
