# -*-Makefile-*-

# version info
MAJVER=0
MINVER=0
PATCHLEVEL=35



####
#### USER CONFIGURABLE
####

#PROFILEFLAG=-pg
OPTIFLAG=-DNDEBUG -DNPRINT -O2
DEBUGFLAG=-g

# uncomment for windhoos
# CXX=g++

# turn off -pipe if linker doesn't support it
EXTRACXXFLAGS=-pipe -Wall -W   -Wmissing-prototypes -DSTRING_UTILS_INLINED

#
# -lefence = ElectricFence.
#
# ElectricFence is a memory debugger which uses the 
# VM hardware to trap malloc/free errors.
#

EXTRALIB+= #-lefence

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



VERSION=$(MAJVER).$(MINVER).$(PATCHLEVEL)
CXXVER=`$(CXX) --version`

# directories
TOPDIR  := $(shell if [ "$$PWD" != "" ]; then echo $$PWD; else pwd; fi)

OBJECTDIR=objects
HEADERDIR=hdr
CCDIR=src
INITDIR=init
DEPDIR=deps
INPUTDIR=input

# 
#
include Sources.make

# UGH, this sux!
gencc=parser.cc lexer.cc
cc=$(mycc) $(gencc)
mym2mgencc=midi-parser.cc midi-lexer.cc
m2mcc=$(mym2mcc) $(mym2mgencc) $(mym2msharedcc)

MY_CCSOURCE=$(addprefix $(CCDIR)/, $(mycc))
CCSOURCE=$(addprefix $(CCDIR)/, $(cc))
obs=$(addprefix $(OBJECTDIR)/,$(cc:.cc=.o)) 
m2mobs=$(addprefix $(OBJECTDIR)/,$(m2mcc:.cc=.o)) 
allcc=$(mycc) $(mym2mcc)
allobs=$(obs) $(m2mobs)
allexe=$(exe) $(m2m)
M2MCCSOURCE=$(addprefix $(CCDIR)/, $(m2mcc))
ALLDEPS=$(addprefix $(DEPDIR)/,$(cc:.cc=.dep))
STABLEOBS=$(addprefix $(OBJECTDIR)/,$(stablecc:.cc=.o)) 
allhdr=$(hdr) $(mym2mhh)
HEADERS=$(addprefix $(HEADERDIR)/,$(allhdr)) 
progdocs=$(HEADERS) $(MY_CCSOURCE)

#dist
.EXPORT_ALL_VARIABLES:

DOCDIR=docxx

PACKAGENAME=lilypond
DNAME=$(PACKAGENAME)-$(VERSION)
M2MNAME=m2m

# distribution files.
othersrc=lexer.l parser.y midi-lexer.l midi-parser.y
SCRIPTS=make_version make_patch genheader clearlily
MAKFILES=Makefile Variables.make Sources.make Initial.make Generate.make \
	configure
OFILES=COPYING README NEWS TODO ANNOUNCE
IFILES=  titledefs.tex lilyponddefs.tex \
	ltest.tex test.tex .dstreamrc dimen.tex 
DFILES=$(MAKFILES) $(OFILES) $(IFILES) $(SCRIPTS)

#compiling
LOADLIBES=-L$(FLOWERDIR) -lflower $(EXTRALIB) -lg++
FLOWERDIR=../flower

CXXFLAGS=$(DEFINES) -I$(HEADERDIR) -I$(FLOWERDIR) $(EXTRACXXFLAGS)
FLEX=flex
BISON=bison
exe=$(PACKAGENAME)
m2m=$(M2MNAME)
OUTPUT_OPTION=$< -o $@
DDIR=$(TOPDIR)/$(DNAME)
SUBDIRS=Documentation $(OBJECTDIR) $(CCDIR) $(HEADERDIR) $(INITDIR) $(DEPDIR) \
	$(INPUTDIR)

depfile=deps/$(subst .o,.dep,$(notdir $@)) 
DODEP=rm -f $(depfile); DEPENDENCIES_OUTPUT="$(depfile) $(OBJECTDIR)/$(notdir $@)"

STRIPDEBUG=true #replace to do stripping of certain objects
LINKER=$(CXX)
include Site.make


