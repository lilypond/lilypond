;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 1998--2022  Han-Wen Nienhuys <hanwen@xs4all.nl>
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
 'accidental-switch-interface
 "Any object that prints one or several accidentals based on alterations."
 '(alteration-glyph-name-alist))

(ly:add-interface
 'ambitus-interface
 "The line between note heads for a pitch range."
 '(gap
   length-fraction
   maximum-gap
   note-heads
   thickness))

(ly:add-interface
 'sticky-grob-interface
 "A grob that is attached to another grob.  Grobs type having this
interface can be either items or spanners, depending on the class
of their host.  Sticky spanners implicitly take their bounds from
the host."
 '(sticky-host))

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
   glyph-left
   glyph-name
   glyph-right
   hair-thickness
   has-span-bar
   kern
   rounded
   segno-kern
   thick-thickness))

(ly:add-interface
 'bar-number-interface
 "A bar number or bar number vertical support object."
 '())

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
 'bend-interface
 "The (curved) line representing a bent string.
Available for the @code{'style} property are @code{'hold}, @code{'pre-bend} and
@code{'pre-bend-hold}.
The following properties may be set in the details list.

@table @code
@item arrow-stencil
The stencil procedure for the @code{BendSpanner} arrow head.
@item curvature-factor
Determines the horizontal part of a bend arrow as percentage of the total
horizontal extent, usually between 0 and@tie{}1.
@item bend-arrowhead-height
The height of the arrow head.
@item bend-arrowhead-width
The width of the arrow head.
@item bend-amount-strings
An alist with entries for @code{'quarter}, @code{'half}, @code{'three-quarter}
and @code{'full}, which are used to print how much a string is bent.
@item curve-x-padding-line-end
For a broken @code{BendSpanner}, set the padding at the line end to subsequent
objects like changed @code{Clef}, etc.
@item curve-y-padding-line-end
For a broken @code{BendSpanner} started from a chord the curves don't match;
there is a certain vertical gap specified by this value.
@item dashed-line-settings
List of three numeric values representing on, off and phase of a dashed line.
@item head-text-break-visibility
A vector of three booleans to set visibility of the arrow head and the text at
a line break.  This is important for @code{'style} set to @code{'hold},
@code{'pre-bend} or @code{'pre-bend-hold}.
@item horizontal-left-padding
The amount of horizontal free space between a @code{TabNoteHead} and the
starting @code{BendSpanner}.
@item successive-level
An integer used as a factor determining the vertical coordinate of the starting
@code{BendSpanner}.  If @code{successive-level} is@tie{}1, the
@code{BendSpanner} starts at the @code{TabNoteHead}.  If consecutive
@code{BendSpanner}s are set this value should be set to an appropriate value for
the first one; later on, this value is maintained by the engraver.
@item target-visibility
A boolean to decide whether the target @code{TabNoteHead} should be visible.
For up-pointing bends this is usually true.
@item y-distance-from-tabstaff-to-arrow-tip
This numeric value determines the distance between the @code{TabStaff} and the
arrow head of the @code{BendSpanner}.
@end table"
 '(details
   style
   direction
   bend-me))

(ly:add-interface
 'bezier-curve-interface
 "A Bézier curve (tie, slur, etc.)."
 '(show-control-points))

(ly:add-interface
 'caesura-script-interface
 "A script for @code{\\caesura}, e.g., an outside-staff comma or a
fermata over a bar line."
 '())

(ly:add-interface
 'centered-bar-number-interface
 "A measure-centered bar number."
 '())

(ly:add-interface
 'centered-bar-number-line-spanner-interface
 "An abstract object used to align centered bar numbers on the same
vertical position."
 '())

(ly:add-interface
 'centered-spanner-interface
 "A spanner that prints a symbol centered between two columns."
 '(self-alignment-X
   spacing-pair))

(ly:add-interface
 'chord-square-interface
 "A chord square in a chord grid."
 '(chord-names
   measure-division
   measure-division-chord-placement-alist
   measure-division-lines-alist))

(ly:add-interface
 'clef-modifier-interface
 "The number describing transposition of the clef, placed below
or above clef sign. Usually this is 8 (octave transposition)
or 15 (two octaves), but LilyPond allows any integer here."
 '(clef-alignments))

(ly:add-interface
 'coda-mark-interface
 "A coda sign."
 '())

(ly:add-interface
 'control-point-interface
 "A grob used to visualize one control point of a Bézier curve
(such as a tie or a slur), for ease of tweaking."
 '(bezier index))

(ly:add-interface
 'control-polygon-interface
 "A grob used to visualize the control polygon of a Bézier curve
(such as a tie or a slur), for ease of tweaking."
 '(bezier extroversion filled))

(ly:add-interface
 'dots-interface
 "The dots to go with a notehead or rest.  @code{direction} sets the preferred
direction to move in case of staff line collisions.  @code{style} defaults to
undefined, which is normal 19th/20th century traditional style.  Set
@code{style} to @code{vaticana} for ancient type dots."
 '(direction dot-count dot-stencil glyph-name style))

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
 'finger-glide-interface
 "The line between @code{Fingering} grobs indicating a glide with that finger.

The property @code{style} may take the following symbols.

@table @code
@item line
A simple connecting line.
@item dashed-line
Print a dashed line.  Customizable with settings for @code{dash-fraction} and
@code{dash-period}.
@item dotted-line
Print a dotted line.
@item stub-right
The printed line is limited to a certain amount right before its right bound.
This amount is configurable by a suitable setting for
@code{bound-details.right.right-stub-length}.
@item stub-left
The printed line is limited to a certain amount right after its left bound.
The amount is configurable by a suitable setting for
@code{bound-details.right.left-stub-length}.
@item stub-both
The printed line combines the settings of @code{stub-left} and
@code{stub-right}.
@item zigzag
A zigzag line, configurable with suitable settings for @code{zigzag-width} and
@code{zigzag-length}.
@item trill
A trill style line.
@item bow
A bow style line.  The orientation of the bow may be tweaked with a suitable
setting of @code{details.bow-direction}.
@end table"

 '(dash-fraction
   dash-period
   details
   style
   zigzag-length
   zigzag-width))

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
   numbering-assertion-function
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
 'grid-chord-name-interface
 "A chord name in a chord grid."
 '(index))

(ly:add-interface
 'staff-highlight-interface
 "A colored span to highlight a music passage."
 '(bound-prefatory-paddings columns elements shorten-pair))

(ly:add-interface
 'horizontal-bracket-text-interface
 "Label for an analysis bracket."
 '(bracket columns))

(ly:add-interface
 'inline-accidental-interface
 "An inlined accidental (i.e., normal accidentals, cautionary
accidentals)."
 '())

(ly:add-interface
 'instrument-specific-markup-interface
 "Instrument-specific markup (like fret boards or harp pedal diagrams)."
 '(fret-diagram-details
   graphical
   harp-pedal-details
   size
   thickness
   woodwind-diagram-details))

(ly:add-interface
 'jump-script-interface
 "A jump instruction, e.g., @emph{D.S.}."
 '())

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
 'lyric-repeat-count-interface
 "A repeat count intended to appear in a line of lyrics."
 '())

(ly:add-interface
 'lyric-space-interface
 "An invisible object that prevents lyric words from being spaced too closely."
 '())

(ly:add-interface
 'lyric-syllable-interface
 "A single piece of lyrics."
 '())

(ly:add-interface
 'mark-interface
 "A rehearsal mark, segno, or coda sign."
 '())

(ly:add-interface
 'measure-counter-interface
 "A counter for numbering measures."
 '(columns
   count-from
   left-number-text
   number-range-separator
   right-number-text))

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
 'musical-paper-column-interface
 "A paper column that is musical.  Paper columns of this variety group
musical items, such as note heads, stems, dots, accidentals, @dots{}"
 '(grace-spacing
   shortest-playing-duration
   shortest-starter-duration
   ))

(ly:add-interface
 'non-musical-paper-column-interface
 "A paper column that is non-musical.  Paper columns of this variety
group breakable items such as clefs, bar lines, time signatures, key
signatures, breathing signs, @dots{}"
 '(between-cols
   break-alignment
   full-measure-extra-space
   line-break-system-details
   line-break-penalty
   line-break-permission
   page-break-penalty
   page-break-permission
   page-turn-penalty
   page-turn-permission))

(ly:add-interface
 'note-name-interface
 "Note names."
 '())

(ly:add-interface
 'number-interface
 "Numbers."
 '(number-type))

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
 'rehearsal-mark-interface
 "A rehearsal mark."
 '())

(ly:add-interface
 'rhythmic-grob-interface
 "Any object with a duration.  Used to determine which grobs are
interesting enough to maintain a hara-kiri staff."
 '())

(ly:add-interface
 'section-label-interface
 "A section label, e.g., @qq{Coda}."
 '())

(ly:add-interface
 'segno-mark-interface
 "A segno."
 '())

(ly:add-interface
 'signum-repetitionis-interface
 "An ancient repeat sign.  It is printed with the same infrastructure
as bar lines, but it is not a bar line."
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
 'spacing-options-interface
 "Supports setting of spacing variables."
 '(spacing-increment
   shortest-duration-space))

(ly:add-interface
 'span-bar-interface
 "A bar line that is spanned between other bar lines.
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
 'text-mark-interface
 "A textual mark."
 '())

(ly:add-interface
 'time-signature-interface
 "A time signature, in different styles.  The following values for
@code{style} are are recognized:

@table @code
@item C
4/4 and 2/2 are typeset as C and struck C, respectively.  All other
time signatures are written with two digits.  The value
@code{default} is equivalent to value @samp{C}.
@item neomensural
2/2, 3/2, 2/4, 3/4, 4/4, 6/4, 9/4, 4/8, 6/8, and 9/8 are typeset
with neo-mensural style mensuration marks.  All other time
signatures are written with two digits.
@item mensural
2/2, 3/2, 2/4, 3/4, 4/4, 6/4, 9/4, 4/8, 6/8, and 9/8 are typeset
with mensural style mensuration marks.  All other time signatures
are written with two digits.
@item single-digit
All time signatures are typeset with a single digit, e.g., 3/2 is
written as@tie{}3.
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
