# title	   package specific variables
# file	   make/Variables.make
#
# do not change this file for site-wide extensions; please use 
# make/$(outdir)/Site.make; 
#
# Any change in files in this directory (make/) would be distributed, if 
# you do make dist 
#

buildscripts = $(depth)/buildscripts


flower-dir = $(depth)/flower
lib-dir = $(depth)/lib
lily-dir = $(depth)/lily
mi2mu-dir = $(depth)/mi2mu
make-dir = $(depth)/make
include-lib = $(depth)/lib/include
include-flower = $(depth)/flower/include



LILYPOND_INCLUDES = -Istinho $(include-lib) $(depth)/lib/$(outdir) $(include-flower) $(depth)/flower/$(outdir) 


# should use to create .spec ?

# installed by 'make installextradoc'
EXTRA_DOC_FILES = \
  ANNOUNCEMENT ANNOUNCE-0.1 AUTHORS.txt BUGS COPYING DEDICATION INSTALL.txt NEWS PATCHES.txt README.txt TODO \
  Documentation/out/*.txt\
  Documentation/tex/*.doc\
  Documentation/tex/*.bib\
  Documentation/pictures/out/lelie_logo.gif\
  input\
  mutopia\

INSTALLED_EXTRA_DOC_FILES = $(addprefix $(prefix:/%=%)/doc/lilypond/, $(EXTRA_DOC_FILES))

# installed by 'make install'
INSTALL_DIST_FILES = \
  bin/convert-mudela\
  bin/mudela-book\
  bin/ly2dvi\
  bin/lilypond$(DOTEXE)\
  bin/mi2mu$(DOTEXE)\
  info/lilypond.info\
  man/man1/mi2mu.1\
  man/man1/lilypond.1\
  man/man1/mudela-book.1\
  man/man1/ly2dvi.1\
  man/man1/convert-mudela.1\
  lib/texmf/texmf/tex/lilypond\
  lib/texmf/texmf/fonts/source/public/lilypond\
  share/lilypond/\
  share/locale/*/LC_MESSAGES/lilypond.mo\


INSTALLED_DIST_FILES = $(addprefix $(prefix:/%=%)/, $(INSTALL_DIST_FILES))

