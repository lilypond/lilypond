MAJVER=1
MINVER=0
PATCHLEVEL=25
PACKAGENAME=flower

#PROFILEFLAG=-pg
DEBUGFLAG= -O2 -DNDEBUG # -g
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
ALLSOURCES=$(hh) $(cc) $(inl) $(templatecc) 
DFILES=$(ALLSOURCES) Makefile Variables.make make_version\
	Sources.make TODO README NEWS
DDIR=$(DNAME)

