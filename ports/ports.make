# mutopia/mutopia.make

#
# Magic: find and include LilyPond's StepMake rules
#
# 0: try local tree
# 1: follow LILYPONDPREFIX
# 2: try source tree in home
# 3: try installed tree in $HOME
# 4: try system installed tree
# 5: try system installed tree
#
make-root=$(wildcard $(depth)/make)
#
make-root?=$(wildcard $(LILYPONDPREFIX)/make)
make-root?=$(wildcard $(HOME)/usr/share/lilypond/make)
make-root?=$(wildcard /usr/share/lilypond/make)
make-root?=$(wildcard /usr/local/share/lilypond/make)
# make-root=<LilyPond's datadir>/make

ifeq ($(SUBDIRS),)
SUBDIRS = $(filter-out .,$(shell find . -maxdepth 1 -type d -not -name 'out*'))
endif

ifneq ($(make-root),)
LOCALSTEPMAKE_TEMPLATES=ports mutopia ly
include $(make-root)/stepmake.make
else
$(error can't find LilyPond's stepmake installation)
endif
#

