# Automatically generated from ./lilypond-0.1.49/make/Makefile.am.wild by bin/autowild
# Do not edit
# project  LilyPond -- the musical typesetter
# title	   zucht
# file	   make/Makefile.am.dist
#
# Copyright (c) 1997 by    
#   	Jan Nieuwenhuizen <jan@digicash.com>
#	Han-Wen Nienhuys <hanwen@stack.nl>
#		...your sort order here, or how to comment-out a comment

IN_FILES = Configure_variables.make.in Makefile.in Toplevel.make.in lelievijver.lsm.in lilypond.lsm.in lilypond.spec.in
MAKE_FILES = Files.make Include.make Initial.make Rules.make Targets.make Template.make User.make Variables.make Version.make
EXTRA_DIST = Makefile.am.wild $(IN_FILES) $(MAKE_FILES) lilypond.spec lilypond.lsm lelievijver.lsm

spec: $(outdir)/lilypond.spec

rpmdocs = $(addprefix Documentation/, $(notdir $(shell ls ../Documentation/$(outdir)/*.txt)))\
	BUGS TODO NEWS DEDICATION ANNOUNCE README
rpmmudocs = $(addprefix Documentation/, $(notdir $(shell ls ../Documentation/*.doc)))
rpmdvis = $(rpmmudocs:.doc=.dvi)
rpmexamples = $(addprefix input/, $(notdir $(shell ls ../input/*.{ly,tex})))

date = $(shell date '+%d%b%y'|tr a-z A-Z)

SED_VERSION= sed 's!@TOPLEVEL_VERSION@!${TOPLEVEL_VERSION}!g'
SED_DATE=sed 's!@DATE@!${date}!g'
SED_EXAMPLES = sed 's!@EXAMPLE_LYS@!${rpmexamples} ${rpmmudocs}!g'
SED_DOCS=sed 's!@TEXT_DOCS@!${rpmdocs}!g'

lilypond.spec: lilypond.spec.in ../VERSION
	cat $< | $(SED_VERSION) | $(SED_DOCS) | $(SED_EXAMPLES) > $@

%.lsm: %.lsm.in ../VERSION
	cat $< | $(SED_VERSION) | $(SED_DATE) > $@

