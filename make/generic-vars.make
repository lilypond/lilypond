# title	   package specific variables
# file	   make/Variables.make
#
# do not change this file for site-wide extensions; please use 
# make/$(outdir)/Site.make; 
#
# Any change in files in this directory (make/) would be distributed, if 
# you do make dist 
#

buildscript-dir = $(src-depth)/buildscripts
script-dir = $(src-depth)/scripts
input-dir = $(src-depth)/input


flower-dir = $(src-depth)/flower
lily-dir = $(src-depth)/lily
mi2mu-dir = $(src-depth)/mi2mu
make-dir = $(src-depth)/make
include-flower = $(src-depth)/flower/include


LILYPOND_INCLUDES = $(include-flower) $(depth)/flower/$(outdir) 


# should use to create .spec ?

# installed by 'make installextradoc'
EXTRA_DOC_FILES = \
  ANNOUNCEMENT ANNOUNCE-0.1 AUTHORS.txt  COPYING DEDICATION INSTALL.txt NEWS PATCHES.txt README.txt TODO \
  Documentation/out/*.txt\
  Documentation/tex/*.doc\
  Documentation/tex/*.bib\
  Documentation/pictures/out/lelie_logo.gif\
  input\

INSTALLED_EXTRA_DOC_FILES = $(addprefix $(prefix:/%=%)/doc/lilypond/, $(EXTRA_DOC_FILES))

# installed by 'make install'
INSTALL_DIST_FILES = \
  bin/convert-ly\
  bin/lilypond-book\
  bin/ly2dvi\
  bin/$(program_prefix)lilypond$(program_suffix)\
  bin/$(program_prefix)mi2mu$(program-suffix)\
  info/lilypond.info\
  man/man1/mi2mu.1\
  man/man1/lilypond.1\
  man/man1/lilypond-book.1\
  man/man1/ly2dvi.1\
  man/man1/convert-ly.1\
  lib/texmf/texmf/tex/lilypond\
  lib/texmf/texmf/fonts/source/public/lilypond\
  share/lilypond/\
  share/locale/*/LC_MESSAGES/lilypond.mo\


INSTALLED_DIST_FILES = $(addprefix $(prefix:/%=%)/, $(INSTALL_DIST_FILES))



