MAJVER=0
MINVER=0
PATCHLEVEL=3

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
IFILES=dimen.tex symbol.ini suzan.ly maartje.ly  lilyponddefs.tex test.tex .dstreamrc
OFILES=Makefile Sources.make depend 
DFILES=$(hdr) $(mycc) $(othersrc) $(OFILES) $(IFILES) $(SCRIPTS) COPYING

#compiling
LOADLIBES=-L$(FLOWERDIR) -lflower
FLOWERDIR=../flower
#DEFINES=-DNDEBUG
CXXFLAGS=$(DEFINES) -I$(FLOWERDIR) -pipe -Wall -g

exe=$(PACKAGENAME)

##################################################################

$(exe): $(obs)
	$(CXX) -o $@ $(obs) $(LOADLIBES)
clean:
	rm -f $(exe) *.o $(DOCDIR)/* core  

distclean: clean
	rm -f TAGS depend version.hh $(gencc) .GENERATE *~

all: kompijl doc

# doc++ documentation of classes
doc:
	-mkdir $(DOCDIR)
	doc++ -p -I -d $(DOCDIR) $(progdocs)

depend: Sources.make  .GENERATE
	$(CXX) $(CXXFLAGS) -MM $(cc) > $@

# hack to create these sources once, before the dependencies
.GENERATE:
	touch .GENERATE depend
	$(MAKE) version.hh
	$(MAKE) $(gencc)
	rm -f depend

include depend

parser.cc: parser.y
	bison -d $<
	mv parser.tab.h parser.hh
	mv parser.tab.c parser.cc

parser.hh: parser.cc

version.o: $(obs) version.hh

version.hh: Makefile make_version
	make_version $(MAJVER) $(MINVER) $(PATCHLEVEL)  > $@

lexer.cc: lexer.l
	flex -+ -t $< > $@

DDIR=$(DNAME)
dist:
	-mkdir $(DDIR)
	ln $(DFILES) $(DDIR)/
	tar cfz $(DNAME).tar.gz $(DDIR)/
	rm -rf $(DDIR)/


TAGS: $(mycc) $(hdr) Sources.make
	etags -CT $(mycc) $(hdr) 


