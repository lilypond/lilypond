;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 1998--2020  Han-Wen Nienhuys <hanwen@xs4all.nl>
;;;;                 Jan Nieuwenhuizen <janneke@gnu.org>
;;;;
;;;; LilyPond is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; LilyPond is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.


;; The interfaces defined here generally have no code (e.g.,
;; dynamic-interface), or have callbacks defined entirely in
;; scheme.  Others interfaces are defined in lily/*.cc with the
;; ADD_INTERFACE function.

;; should include default value?


(ly:add-interface
 'accidental-suggestion-interface
 "An accidental, printed as a suggestion (typically: vertically over a
note)."
 '())

(ly:add-interface
 'ambitus-interface
 "The line between note heads for a pitch range."
 '(gap
   length-fraction
   maximum-gap
   note-heads
   thickness))

(ly:add-interface
 'bar-line-interface
 "Print a special bar symbol.  It replaces the regular bar
symbol with a special symbol.  The argument @var{bartype}
is a string which specifies the kind of bar line to print.

The list of allowed glyphs and predefined bar lines can be
found in @file{scm/bar-line.scm}.

@code{gap} is used for the gaps in dashed bar lines."
 '(allow-span-bar
   bar-extent
   gap
   glyph
   glyph-name
   hair-thickness
   has-span-bar
   kern
   rounded
   segno-kern
   thick-thickness))

(ly:add-interface
 'bass-figure-interface
 "A bass figure text."
 '(implicit))

(ly:add-interface
 'bass-figure-alignment-interface
 "Align a bass figure."
 '())

(ly:add-interface
 'bend-after-interface
 "A doit or drop."
 '(delta-position
   thickness))

(ly:add-interface
 'clef-modifier-interface
 "The number describing transposition of the clef, placed below
or above clef sign. Usually this is 8 (octave transposition)
or 15 (two octaves), but LilyPond allows any integer here."
 '(clef-alignments))

(ly:add-interface
 'duration-line-interface
 "A line lasting for the duration of a rhythmic event."
 '(details))

(ly:add-interface
 'dynamic-interface
 "Any kind of loudness sign."
 '())

(ly:add-interface
 'dynamic-line-spanner-interface
 "Dynamic line spanner."
 '(avoid-slur))

(ly:add-interface
 'dynamic-text-interface
 "An absolute text dynamic."
 '(right-padding))

(ly:add-interface
 'dynamic-text-spanner-interface
 "Dynamic text spanner."
 '(text))

(ly:add-interface
 'episema-interface
 "An episema line."
 '())

(ly:add-interface
 'finger-interface
 "A fingering instruction."
 '())

(ly:add-interface
 'footnote-interface
 "Make a footnote."
 '(automatically-numbered
   footnote
   footnote-text
   numbering-assertion-function))

(ly:add-interface
 'footnote-spanner-interface
 "Make a footnote spanner."
 '(footnote-text
   spanner-placement))

(ly:add-interface
 'fret-diagram-interface
 "A fret diagram"
 '(align-dir
   dot-placement-list
   fret-diagram-details
   size
   thickness))

(ly:add-interface
 'glissando-interface
 "A glissando."
 '(glissando-index))

(ly:add-interface
 'grace-spacing-interface
 "Keep track of durations in a run of grace notes."
 '(columns
   common-shortest-duration))

(ly:add-interface
 'horizontal-bracket-text-interface
 "Label for an analysis bracket."
 '(bracket columns))

(ly:add-interface
 'inline-accidental-interface
 "An inlined accidental (i.e. normal accidentals, cautionary
accidentals)."
 '())

(ly:add-interface
 'instrument-specific-markup-interface
 "Instrument-specific markup (like fret boards or harp pedal diagrams)."
 '(fret-diagram-details graphical harp-pedal-details size thickness))

(ly:add-interface
 'key-cancellation-interface
 "A key cancellation."
 '())

(ly:add-interface
 'ligature-head-interface
 "A note head that can become part of a ligature."
 '())

(ly:add-interface
 'ligature-bracket-interface
 "A bracket indicating a ligature in the original edition."
 '(width thickness height))

(ly:add-interface
 'ligature-interface
 "A ligature."
 '())

(ly:add-interface
 'lyric-interface
 "Any object that is related to lyrics."
 '())

(ly:add-interface
 'lyric-syllable-interface
 "A single piece of lyrics."
 '())

(ly:add-interface
 'mark-interface
 "A rehearsal mark."
 '())

(ly:add-interface
 'measure-counter-interface
 "A counter for numbering measures."
 '(columns
   count-from
   spacing-pair))

(ly:add-interface
 'metronome-mark-interface
 "A metronome mark."
 '())

(ly:add-interface
 'multi-measure-interface
 "Multi measure rest, and the text or number that is printed over it."
 '(bound-padding))

(ly:add-interface
 'multi-measure-rest-number-interface
 "Multi measure rest number that is printed over a rest."
 '())

(ly:add-interface
 'note-name-interface
 "Note names."
 '())

(ly:add-interface
 'number-interface
 "Numbers."
 '(number-type))

(ly:add-interface
 'only-prebreak-interface
 "Kill this grob after the line breaking process."
 '())

(ly:add-interface
 'outside-staff-axis-group-interface
 "A vertical axis group on which outside-staff skyline calculations are done."
 '(outside-staff-placement-directive vertical-skyline-elements))

(ly:add-interface
 'outside-staff-interface
 "A grob that could be placed outside staff."
 '(outside-staff-horizontal-padding outside-staff-padding outside-staff-priority))

(ly:add-interface
 'parentheses-interface
 "Parentheses for other objects."
 '(padding
   stencils))

(ly:add-interface
 'percent-repeat-interface
 "Beat, Double and single measure repeats."
 '(dot-negative-kern
   slash-negative-kern
   slope
   thickness))

(ly:add-interface
 'piano-pedal-interface
 "A piano pedal sign."
 '())

(ly:add-interface
 'piano-pedal-script-interface
 "A piano pedal sign, fixed size."
 '())

(ly:add-interface
 'pitched-trill-interface
 "A note head to indicate trill pitches."
 '(accidental-grob))

(ly:add-interface
 'rhythmic-grob-interface
 "Any object with a duration.  Used to determine which grobs are
interesting enough to maintain a hara-kiri staff."
 '())

(ly:add-interface
 'spacing-options-interface
 "Supports setting of spacing variables."
 '(spacing-increment
   shortest-duration-space))

(ly:add-interface
 'span-bar-interface
 "A bar line that is spanned between other barlines.
  This interface is used for bar lines that connect different
  staves."
 '(glyph-name
   elements
   pure-Y-common
   pure-relevant-grobs
   pure-relevant-items
   pure-relevant-spanners))

(ly:add-interface
 'stanza-number-interface
 "A stanza number, to be put in from of a lyrics line."
 '())

(ly:add-interface
 'string-number-interface
 "A string number instruction."
 '())

(ly:add-interface
 'stroke-finger-interface
 "A right hand finger instruction."
 '(digit-names))

(ly:add-interface
 'system-start-text-interface
 "Text in front of the system."
 '(long-text
   self-alignment-X
   self-alignment-Y
   text))

(ly:add-interface
 'tab-note-head-interface
 "A note head in tablature."
 '(details display-cautionary span-start))

(ly:add-interface
 'time-signature-interface
 "A time signature, in different styles.  The following values for @code{style} are are recognized:

 @table @code
 @item C
 4/4 and 2/2 are typeset as C and struck C, respectively. All other time signatures are written with two digits. The value @code{default} is equivalent to @code{C}.
 @item neomensural
 2/2, 3/2, 2/4, 3/4, 4/4, 6/4, 9/4, 4/8, 6/8, and 9/8 are typeset with neo-mensural style mensuration marks.  All other time signatures are written with two digits.
 @item mensural
 2/2, 3/2, 2/4, 3/4, 4/4, 6/4, 9/4, 4/8, 6/8, and 9/8 are typeset with mensural style mensuration marks.  All other time signatures are written with two digits.
 @item single-digit
 All time signatures are typeset with a single digit, e.g., 3/2 is written as 3.
 @item numbered
 All time signatures are typeset with two digits.
 @end table"
 '(fraction style))

(ly:add-interface
 'trill-spanner-interface
 "A trill spanner."
 '())

(ly:add-interface
 'trill-pitch-accidental-interface
 "An accidental for trill pitch."
 '())

(ly:add-interface
 'unbreakable-spanner-interface
 "A spanner that should not be broken across line breaks.  Override
with @code{breakable=##t}."
 '(breakable))

(ly:add-interface
 'volta-interface
 "A volta repeat."
 '())
