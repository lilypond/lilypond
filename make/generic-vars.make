# title	   package specific variables
# file	   make/Variables.make
#
# do not change this file for site-wide extensions; please use
# make/$(outdir)/Site.make;
#
# Any change in files in this directory (make/) would be distributed, if
# you do make dist
#

buildscript-dir = $(top-build-dir)/scripts/build/$(outconfbase)
auxpython-dir = $(src-depth)/python/auxiliar
auxscript-dir = $(src-depth)/scripts/auxiliar
script-dir = $(src-depth)/scripts
input-dir = $(src-depth)/input

flower-dir = $(src-depth)/flower
lily-dir = $(src-depth)/lily
mi2mu-dir = $(src-depth)/mi2mu
make-dir = $(src-depth)/make
include-flower = $(src-depth)/flower/include

export PYTHONPATH:=$(auxpython-dir):$(PYTHONPATH)

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


INSTALLED_DIST_FILES = $(addprefix $(prefix:/%=%)/, $(INSTALL_DIST_FILES))



