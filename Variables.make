####
#### USER CONFIGURABLE
####

#PROFILEFLAG=-pg
OPTIFLAG=-DNDEBUG -DNPRINT -O2
DEBUGFLAG=-g
# turn off -pipe if linker doesn't support it
EXTRACXXFLAGS=-pipe -Wall -W   -Wmissing-prototypes 
#	-Woverloaded-virtual

#
# -lefence = ElectricFence.
#
# ElectricFence is memory debugger which uses the 
# VM hardware to trap malloc/free errors.
#
#EXTRALIB+=-lefence

####
#### END USER CONFIGURABLE part.
####

ifdef PROFILEFLAG
	DEFINES+=$(OPTIFLAG) $(PROFILEFLAG)
	EXTRALIB+=-pg
endif

ifndef DEBUGFLAG
	DEFINES+=$(OPTIFLAG)
else
	DEFINES+=$(DEBUGFLAG)
endif



# version info
MAJVER=0
MINVER=0
PATCHLEVEL=17
VERSION=$(MAJVER).$(MINVER).$(PATCHLEVEL)
CXXVER=`$(CXX) --version`

# directories
TOPDIR  := $(shell if [ "$$PWD" != "" ]; then echo $$PWD; else pwd; fi)
OBJECTDIR=objects
HEADERDIR=hdr
CCDIR=src

vpath %.cc $(CCDIR)
vpath %.hh $(HEADERDIR)
vpath %.y $(CCDIR)
vpath %.l $(CCDIR)
vpath %.o $(OBJECTDIR)

# 
#
include Sources.make
progdocs=$(hdr) $(mycc)
gencc=parser.cc lexer.cc
cc=$(mycc) $(gencc)
obs=$(cc:.cc=.o) 


#dist
.EXPORT_ALL_VARIABLES:

DOCDIR=docdir

PACKAGENAME=lilypond
DNAME=$(PACKAGENAME)-$(VERSION)

# distribution files.
othersrc=lexer.l parser.y
SCRIPTS=make_version make_patch genheader
IFILES=dimen.tex symbol.ini kortjakje.ly pavane.ly  maartje.ly\
	lilyponddefs.tex test.tex .dstreamrc
OFILES=Makefile Variables.make Sources.make COPYING README NEWS
DFILES=$(OFILES) $(IFILES) $(SCRIPTS)

#compiling
LOADLIBES=-L$(FLOWERDIR) -lflower $(EXTRALIB)
FLOWERDIR=../flower

CXXFLAGS=$(DEFINES) -I$(HEADERDIR) -I$(FLOWERDIR) $(EXTRACXXFLAGS)
FLEX=flex
BISON=bison
exe=$(PACKAGENAME)
OUTPUT_OPTION=$< -o $@
DDIR=$(TOPDIR)/$(DNAME)
SUBDIRS=Documentation $(OBJECTDIR) $(CCDIR) $(HEADERDIR)
