# mutopia/mutopia.make

#
# Magic: find and include LilyPond's StepMake rules
#
# 0: follow LILYPONDPREFIX
# 1: try source tree
# 2: try installed tree in $HOME
# 3: try system installed tree
#
make-root=$(wildcard $(LILYPONDPREFIX)/make)
make-root?=$(wildcard $(HOME)/usr/src/lilypond/make)
make-root?=$(wildcard /usr/share/lilypond/make)
make-root?=$(wildcard /usr/share/lilypond/make)
#make-root=<LilyPond's datadir>/make
ifneq ($(make-root),)
### some versions apparently choke on $(message)
### $(message running from $(make-root))
depth=$(make-root)/..
LOCALSTEPMAKE_TEMPLATES=ly mutopia
include $(make-root)/stepmake.make
else
$(error can't find LilyPond's stepmake installation)
endif
#

