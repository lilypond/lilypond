#!/usr/bin/make
#
# Mutopia Makefile Project
#
# Rename this file to GNUmakefile, and issue `make help'
#


#
# Magic: find and include LilyPond's StepMake rules
#
# First, try source tree
# Second, try installed tree in $HOME
# Third, try system installed tree
#
#
##For testing installed lily
##make-root=$(wildcard $(HOME)/tmp/test/usr/share/lilypond/make)
#
#
make-root=$(wildcard $(HOME)/usr/src/lilypond/make)
make-root?=$(wildcard /usr/share/lilypond/make)
make-root?=$(wildcard /usr/share/lilypond/make)
ifneq ($(make-root),)
$(message running from $(make-root))
depth=$(make-root)/..
LOCALSTEPMAKE_TEMPLATES=ly mutopia
include $(make-root)/stepmake.make
else
$(error can't find LilyPond's stepmake installation)
endif
#


#
# Mutopia/user targets.
# This needs some work.
#

parts=$(patsubst %.ly,%,$(wildcard *-part.ly))
tarball=$(name)
mutopia-examples=$(name)

