# Automatically generated from ./lilypond-0.1.49/lily/Makefile.am.wild by bin/autowild
# Do not edit
# project  LilyPond -- the musical typesetter # title	   makefile for lilypond
# file	   lily/Makefile.am.wild
#
# Copyright (c) 1997 by
#   	Jan Nieuwenhuizen <jan@digicash.com>
#	Han-Wen Nienhuys <hanwen@stack.nl>
#

SUBDIRS = include

bin_PROGRAMS = lilypond

# ignoring wildcards for `lexer.cc'
# ignoring wildcards for `parser.cc'
 
lilypond_SOURCES = abbrev.cc abbreviation-beam-engraver.cc abbreviation-beam.cc atom.cc audio-column.cc audio-element.cc audio-item.cc audio-staff.cc axis-group-administration.cc axis-group-element.cc axis-group-item.cc axis-group-spanner.cc axis.cc bar-column-grav.cc bar-column.cc bar-grav.cc bar-number-grav.cc bar.cc beam-grav.cc beam-swallow-trans.cc beam.cc bow.cc boxes.cc break-align-item.cc break.cc change-iterator.cc change-translator.cc chord-iterator.cc clef-grav.cc clef-item.cc col-info.cc colhpos.cc collision-grav.cc collision.cc command-request.cc crescendo.cc debug.cc dimen.cc directional-spanner.cc dot-column-grav.cc dot-column.cc dots.cc dynamic-grav.cc elem-group.cc engraver-group.cc engraver.cc general-script-def.cc global-translator.cc gourlay-breaking.cc graphical-element.cc grouping.cc head-grav.cc header.cc horizontal-align-item.cc horizontal-group-item.cc horizontal-vertical-group-item.cc idealspacing.cc identifier.cc ineq-constrained-qp.cc item.cc key-grav.cc key-item.cc key-performer.cc key.cc keyword.cc leastsquares.cc lexerinit.cc lily-version.cc line-group-grav.cc line-spacer.cc local-key-grav.cc local-key-item.cc lookup.cc lyric-grav.cc lyric-performer.cc main.cc meter-grav.cc meter-performer.cc meter.cc midi-def.cc midi-item.cc midi-stream.cc midi-walker.cc misc.cc molecule.cc music-iterator.cc music-list.cc music-output-def.cc music.cc musical-request.cc my-lily-lexer.cc my-lily-parser.cc note-column.cc note-head.cc note-performer.cc note.cc notename-table.cc offset.cc outputter.cc p-col.cc p-score.cc paper-def.cc performance.cc performer-group-performer.cc performer.cc pitch-squash-grav.cc plet-engraver.cc plet-spanner.cc plet-swallow-engraver.cc priority-halign-grav.cc property-iterator.cc ps-plet.cc qlp.cc qlpsolve.cc request-iterator.cc request.cc rest-collision-grav.cc rest-collision.cc rest-grav.cc rest.cc rhythmic-column-grav.cc rhythmic-head.cc score-bar.cc score-column.cc score-elem-info.cc score-elem.cc score-grav.cc score-performer.cc score-priority-grav.cc score.cc scoreline.cc scores.cc script-column.cc script-def.cc script-grav.cc script.cc slur-grav.cc slur.cc span-bar-grav.cc span-bar.cc span-score-bar-grav.cc span-score-bar.cc spanner.cc spring-spacer.cc staff-info.cc staff-performer.cc staff-side.cc staff-sym-grav.cc staff-sym.cc stem-grav.cc stem-info.cc stem.cc super-elem.cc swallow-grav.cc swallow-perf.cc symtable.cc template1.cc template2.cc template3.cc template4.cc template5.cc template6.cc template7.cc template8.cc tex-beam.cc tex-slur.cc tex-stream.cc tex.cc text-def.cc text-item.cc text-spanner.cc tie-grav.cc tie.cc time-description.cc timing-grav.cc timing-translator.cc translation-property.cc translator-ctors.cc translator-group.cc translator.cc type-swallow-trans.cc version.cc vertical-align-elem.cc vertical-align-grav.cc vertical-align-spanner.cc vertical-group-spanner.cc voice-iterator.cc warn.cc word-wrap.cc lexer.ll parser.yy
noinst_HEADERS = parser.hh

lilypond_LDADD = -llily -lflower @LEXLIB@

lilypond_LDFLAGS = -L../lib -L../flower

INCLUDES = -I. -Iinclude -I../lib/include -I../flower/include 

EXTRA_DIST = Makefile.am.wild Stable.make VERSION

# force these: Make can't know these have to be generated in advance
my-lily-lexer.cc: parser.hh
lexer.cc: parser.hh

# ugh, autoconf assumes to get $(LEX_OUTPUT_ROOT).c ...
.ll.cc:
	@echo "must override autoconf; ignore warning"
	$(LEX) $(LFLAGS) $< && mv $(LEX_OUTPUT_ROOT).cc $@
#.yy.hh:
parser.hh: parser.yy
	$(BISON) -d $<
	mv $<.tab.h $@
	mv $<.tab.c $(shell basename $@ .hh).cc
# .ll.cc:
# ugh, stupid automake 
# now set output root by hand...
#	touch lex.yy.cc

# .yy.cc:
# 	$(BISON) $<
# 	mv $(shell basename $@ .cc ).tab.c $@
# 	$(FLEX) -Cfe -p -p -t $< > $@
# # could be faster:
# #	$(FLEX) -8 -Cf -t $< > $@

