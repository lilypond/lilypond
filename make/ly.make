#!/usr/bin/make
#
# Mutopia Makefile Project
#
# Rename this file to GNUmakefile, and issue `make help'
#


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


#
# Mutopia/user targets.
# This needs some work.
#

#
# Name of mutopia project
#
name=book
tarball=$(name)
parts=$(patsubst %.ly,%,$(wildcard *-part.ly))

#
# scores for target local-WWW (duh)
#
examples=

#
# scores for target mutopia
#
mutopia-examples=$(name) $(parts)

#
# Timothy's booklet
#
$(outdir)/%-book.ps: $(outdir)/%.ps
	psbook $< $<.tmp
	pstops '2:0L(11.45in,0.25in)+1L(11.45in,5.6in)' $<.tmp $@

#
# Catch-all target: type `make foo' to make out/foo.ps,
# or make `foo-book' to make out/foo-book.ps
#
%: $(outdir)/%.ps
	@echo Making $@ from $<

#
# Also clean hand-compiled stuff in cwd
#
local-clean: local-auto-gen-clean

# Compose string from two parts: must not remove myself.
auto-gen-tag=Generated
auto-gen-tag+= automatically by

local-auto-gen-clean:
	rm -f `grep -l '$(auto-gen-tag)' *`
	rm -f *.dvi *.png

