####
#### USER CONFIGURABLE
####

# speedy
#DEFINES=-DNDEBUG -DNPRINT -O2

# lots of debugging info
DEFINES=-g

# turn off -pipe if linker doesn't support it
EXTRACXXFLAGS=-pipe -Wall -W  -pedantic 

####
#### EN USER CONFIGURABLE part.
####

# version info
MAJVER=0
MINVER=0
PATCHLEVEL=10
VERSION=$(MAJVER).$(MINVER).$(PATCHLEVEL)

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
IFILES=dimen.tex symbol.ini kortjakje.ly maartje.ly\
	lilyponddefs.tex test.tex .dstreamrc
OFILES=Makefile Variables.make Sources.make COPYING README
DFILES=$(OFILES) $(IFILES) $(SCRIPTS)

#compiling
LOADLIBES=-L$(FLOWERDIR) -lflower
FLOWERDIR=../flower

CXXFLAGS=$(DEFINES) -I$(HEADERDIR) -I$(FLOWERDIR) $(EXTRACXXFLAGS)
FLEX=flex
BISON=bison
exe=$(PACKAGENAME)
OUTPUT_OPTION=$< -o $@
DDIR=$(TOPDIR)/$(DNAME)
SUBDIRS=Documentation $(OBJECTDIR) $(CCDIR) $(HEADERDIR)
