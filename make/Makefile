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
SUBDIRS = 

# two outdir files are distributed, since they make sense to have without running 
# configure and make.

LSMENTRY=$(outdir)/lilypond.lsm
RPMSPEC=$(outdir)/lilypond.spec 
IN_FILES = $(wildcard *.in)
EXTRA_DISTFILES = $(IN_FILES)
OUT_DISTFILES= $(outdir)/lelievijver.lsm $(LSMENTRY) $(RPMSPEC)
#

# generic variables:
#
include ./$(depth)/make/Variables.make
include ./$(depth)/make/Files.make 
include ./$(depth)/make/Targets.make
include ./$(depth)/make/Rules.make

default: $(RPMSPEC) $(LSMENTRY)


rpmdocs=BUGS TODO NEWS DEDICATION ANNOUNCE README
rpmdvis=$(rpmmudocs:.doc=.dvi)
rpmexamples= $(addprefix input/, $(notdir $(shell ls $(depth)/input/*.ly)))


date = $(shell date '+%d%b%y'|tr a-z A-Z)
sed-version= sed 's!@TOPLEVEL_VERSION@!${TOPLEVEL_VERSION}!g'
sed-date=sed 's!@DATE@!${date}!g'
sed-examples = sed 's!@EXAMPLE_LYS@!${rpmexamples} ${rpmmudocs}!g'
sed-docs=sed 's!@TEXT_DOCS@!${rpmdocs}!g'

$(outdir)/%.spec: %.spec.in $(depth)/VERSION
	cat $< | $(sed-version) | $(sed-docs) | $(sed-examples) > $@

$(outdir)/%.lsm: %.lsm.in $(depth)/VERSION
	cat $< | $(sed-version) | $(sed-date) > $@


