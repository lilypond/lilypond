
# -> mutopia-vars.make
MUTOPIA_MIRROR = http://www.mutopiaproject.org/ftp
# ugh: doesn't work
# mutopia-dir = $(pwd:%/mutopia/%=mutopia)
mutopia-dir = $(shell pwd | sed 's@.*mutopia@@')


wget-list = $(mutopia-examples:%=$(mutopia-dir)/%)

dirs-ignore = GNUmakefile out out-% index.html ftp
dirs-before = $(SUBDIRS)
dirs-after = $(filter-out .,$(shell find . -maxdepth 1 -type d -not -name 'out*'))


GNUmakefile = '\
depth = $(depth)/..\n\
include $$(depth)/ports/ports.make\n\
'


#
# scores for target local-WWW (duh)
#
examples=


#
# scores for target mutopia
#
# Hairy hack to support name `score.ly' inside zipfile.
# This will fail to download and build mutopia in one go,
# either download first, or issue `make mutopia' twice.
# How to really fix this?
#
mutopia-parts = $(patsubst %.ly,%,$(wildcard *-part.ly))
mutopia-scores = $(patsubst %.ly,%,$(wildcard $(mutopia-name).ly score.ly))
ifeq ($(mutopia-scores),)
mutopia-scores = $(patsubst %.ly,%, $(shell grep -l '\\score' *.ly))
ifeq ($(mutopia-scores),)
mutopia-scores = $(mutopia-name)
endif
endif
mutopia-examples = $(sort $(mutopia-scores) $(mutopia-parts))

