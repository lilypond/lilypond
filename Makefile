MAJVER=0
MINVER=0
PATCHLEVEL=2

# 
#

include Sources.make
progdocs=$(hdr) $(mycc)
gencc=parser.cc lexer.cc
cc=$(mycc) $(gencc)
obs=$(cc:.cc=.o)


#dist

DOCDIR=docdir
VERSION=$(MAJVER).$(MINVER).$(PATCHLEVEL)
PACKAGENAME=lilypond
DNAME=$(PACKAGENAME)-$(VERSION)
othersrc=lexer.l parser.y
SCRIPTS=make_version make_patch
IFILES= symbol.ini suzan.ly  lilyponddefs.tex test.tex .dstreamrc
OFILES=Makefile Sources.make depend 
DFILES=$(hdr) $(mycc) $(othersrc) $(OFILES) $(IFILES) $(SCRIPTS)

#compiling
LOADLIBES=-L$(FLOWERDIR) -lflower
FLOWERDIR=../flower
CXXFLAGS=-I$(FLOWERDIR) -pipe -Wall -g

exe=$(PACKAGENAME)



$(exe): $(obs)
	$(CXX) -o $(exe) $(obs) $(LOADLIBES)
clean:
	rm -f $(exe) *.o $(DOCDIR)/* core

realclean: clean
	rm -f TAGS depend
	
all: kompijl doc

# doc++ documentation of classes
doc:
	-mkdir $(DOCDIR)
	doc++ -p -I -d $(DOCDIR) $(progdocs)

back:
	zip -u ~/backs/spacer *cc *hh

depend: Sources.make 
	$(CXX) $(CXXFLAGS) -MM $(cc) > depend

include depend

parser.cc: parser.y
	bison -d $<
	mv parser.tab.h parser.hh
	mv parser.tab.c parser.cc

parser.hh: parser.cc

version.o: $(obs) version.hh

version.hh: Makefile make_version
	make_version $(MAJVER) $(MINVER) $(PATCHLEVEL)  > version.hh

lexer.cc: lexer.l
	flex -+ -t lexer.l > lexer.cc

DDIR=$(DNAME)
dist:
	-mkdir $(DDIR)
	ln $(DFILES) $(DDIR)/
	tar cfz $(DNAME).tar.gz $(DDIR)/*
	rm -rf $(DDIR)/


TAGS: $(mycc) $(hdr) Sources.make
	etags -CT $(mycc) $(hdr) 
