# title	   package specific variables
# file	   make/Variables.make
#
# do not change this file for site-wide extensions; please use 
# make/$(outdir)/Site.make; 
#
# Any change in files in this directory (make/) would be distributed, if 
# you do make dist 
#

# derived names
flowerout = $(buildprefix)/flower/$(outdir)
libout = $(buildprefix)/lib/$(outdir)
lilyout = $(buildprefix)/lily/$(outdir)
mi2muout = $(buildprefix)/mi2mu/$(outdir)

buildscripts = $(depth)/buildscripts


flower-dir = $(depth)/flower
lib-dir = $(depth)/lib
lily-dir = $(depth)/lily
mi2mu-dir = $(depth)/mi2mu
make-dir = $(depth)/make
include-lib = $(depth)/lib/include
include-flower = $(depth)/flower/include

NO_DOOS_DIST = flower lib lily make mi2mu out

# dummydeps
#
DUMMYDEPS=\
 $(flowerout)/dummy.dep\
 $(libout)/dummy.dep\
 $(lilyout)/dummy.dep\
 $(mi2muout)/dummy.dep\

#

# version stuff:
#
lily-version = $(lilyout)/version.hh
flower-version = $(flowerout)/version.hh
mi2mu-version = $(mi2muout)/version.hh
#

# custom libraries:
#
LIBFLOWER = $(flowerout)/$(LIB_PREFIX)flower$(LIB_SUFFIX)
LIBLILY = $(libout)/$(LIB_PREFIX)lily$(LIB_SUFFIX)
#

LILYPOND_INCLUDES = -I$(include-lib) -I$(libout) -I$(include-flower) -I$(flowerout) 
LILYPOND_LDFLAGS = -L$(depth)/lib/$(outdir) -L$(depth)/flower/$(outdir)
LILYPOND_LIBES =


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

