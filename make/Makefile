#
# project  LilyPond -- the musical typesetter
# title	   zucht
# file	   make/Makefile 
#
# Copyright (c) 1997 by    
#   	Jan Nieuwenhuizen <jan@digicash.com>
#	Han-Wen Nienhuys <hanwen@stack.nl>
#		...your sort order here, or how to comment-out a comment

# subdir level:
#
depth = ..
#

# identify module:
#
NAME = make
SUBDIRS = 

# list of distribution files:

# two outdir files are distributed, since they make sense to have without running 
# configure and make.
IN_FILES = $(wildcard *.in)
EXTRA_DISTFILES = $(IN_FILES) $(outdir)/lilypond.spec $(outdir)/lilypond.lsm $(outdir)/lelievijver.lsm
#

# generic variables:
#
include ./$(depth)/make/Variables.make
include ./$(depth)/make/Files.make 
#

# generic targets and rules:
#
include ./$(depth)/make/Targets.make
include ./$(depth)/make/Rules.make
#

spec: $(outdir)/lilypond.spec

rpmdocs=$(addprefix Documentation/out/, $(notdir $(shell ls $(depth)/Documentation/$(outdir)/*.txt)))\
	BUGS TODO NEWS DEDICATION ANNOUNCE README
rpmmudocs=$(addprefix Documentation/, $(notdir $(shell ls $(depth)/Documentation/*.doc)))
rpmdvis=$(rpmmudocs:.doc=.dvi)
rpmexamples= $(addprefix input/, $(notdir $(shell ls $(depth)/input/*.{ly,tex})))


date = $(shell date '+%d%b%y'|tr a-z A-Z)
sed-version= sed 's!@TOPLEVEL_VERSION@!${TOPLEVEL_VERSION}!g'
sed-date=sed 's!@DATE@!${date}!g'
sed-examples = sed 's!@EXAMPLE_LYS@!${rpmexamples} ${rpmmudocs}!g'
sed-docs=sed 's!@TEXT_DOCS@!${rpmdocs}!g'

$(outdir)/lilypond.spec: lilypond.spec.in $(depth)/VERSION
	cat $< | $(sed-version) | $(sed-docs) | $(sed-examples) > $@

$(outdir)/%.lsm: %.lsm.in $(depth)/VERSION
	cat $< | $(sed-version) | $(sed-date) > $@

