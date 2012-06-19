;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 1998--2012  Han-Wen Nienhuys <hanwen@xs4all.nl>
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

(define (define-grob-property symbol type? description)
  (if (not (equal? (object-property symbol 'backend-doc) #f))
      (ly:error (_ "symbol ~S redefined") symbol))

  (set-object-property! symbol 'backend-type? type?)
  (set-object-property! symbol 'backend-doc description)
  symbol)

;; put this in an alist?
(define-public all-user-grob-properties
  (map
   (lambda (x)
     (apply define-grob-property x))

   `(
;;;
;;; a
;;;
     (add-stem-support ,boolean? "If set, the @code{Stem} object is
included in this script's support.")
     (after-line-breaking ,boolean? "Dummy property, used to trigger
callback for @code{after-line-breaking}.")
     (align-dir ,ly:dir? "Which side to align? @w{@code{-1}}: left side,
@code{0}: around center of width, @code{1}: right side.")
     (allow-loose-spacing ,boolean? "If set, column can be detached
from main spacing.")
     (allow-span-bar ,boolean? "If false, no inter-staff bar line will
be created below this bar line.")
     (alteration ,number? "Alteration numbers for accidental.")
     (alteration-alist ,list? "List of @code{(@var{pitch}
. @var{accidental})} pairs for key signature.")
     (annotation ,string? "Annotate a grob for debug purposes.")
     (annotation-balloon ,boolean? "Print the balloon around an annotation.")
     (annotation-line ,boolean? "Print the line from an annotation to the
grob that it annotates.")
     (arpeggio-direction ,ly:dir? "If set, put an arrow on the
arpeggio squiggly line.")
     (arrow-length ,number? "Arrow length.")
     (arrow-width ,number? "Arrow width.")
     (auto-knee-gap ,ly:dimension? "If a gap is found between note
heads where a horizontal beam fits that is larger than this number,
make a kneed beam.")
     (automatically-numbered ,boolean? "Should a footnote be automatically
numbered?")
     (average-spacing-wishes ,boolean? "If set, the spacing wishes are
averaged over staves.")
     (avoid-note-head ,boolean? "If set, the stem of a chord does not
pass through all note heads, but starts at the last note head.")
     (avoid-scripts ,boolean? "If set, a tuplet bracket avoids the
scripts associated with the note heads it encompasses.")
     (avoid-slur ,symbol? "Method of handling slur collisions.
Choices are @code{inside}, @code{outside}, @code{around}, and
@code{ignore}.  @code{inside} adjusts the slur if needed to keep the
grob inside the slur.  @code{outside} moves the grob vertically to the
outside of the slur.  @code{around} moves the grob vertically to the
outside of the slur only if there is a collision.  @code{ignore} does
not move either.  In grobs whose notational significance depends on
vertical position (such as accidentals, clefs, etc.), @code{outside}
and @code{around} behave like @code{ignore}.")
     (axes ,list? "List of axis numbers.  In the case of alignment
grobs, this should contain only one number.")


;;;
;;; b
;;;
     (bar-extent ,number-pair? "The Y-extent of the actual bar line.
This may differ from @code{Y-extent} because it does not include the
dots in a repeat bar line.")
     (base-shortest-duration ,ly:moment? "Spacing is based on the
shortest notes in a piece.  Normally, pieces are spaced as if notes at
least as short as this are present.")
     (baseline-skip ,ly:dimension? "Distance between base lines of
multiple lines of text.")
     (beam-thickness ,ly:dimension? "Beam thickness, measured in
@code{staff-space} units.")
     (beam-width ,ly:dimension? "Width of the tremolo sign.")
     (beamed-stem-shorten ,list? "How much to shorten beamed stems,
when their direction is forced.  It is a list, since the value is
different depending on the number of flags and beams.")
     (beaming ,pair? "Pair of number lists.  Each number list
specifies which beams to make.  @code{0}@tie{}is the central beam,
@code{1}@tie{}is the next beam toward the note, etc.  This information
is used to determine how to connect the beaming patterns from stem to
stem inside a beam.")
     (beamlet-default-length ,pair? "A pair of numbers.  The first
number specifies the default length of a beamlet that sticks out of
the left hand side of this stem; the second number specifies the
default length of the beamlet to the right.  The actual length of a
beamlet is determined by taking either the default length or the
length specified by @code{beamlet-max-length-proportion}, whichever is
smaller.")
     (beamlet-max-length-proportion ,pair? "The maximum length of a
beamlet, as a proportion of the distance between two adjacent stems.")
     (before-line-breaking ,boolean? "Dummy property, used to trigger
a callback function.")
     (between-cols ,pair? "Where to attach a loose column to.")
     (bound-details ,list? "An alist of properties for determining
attachments of spanners to edges.")
     (bound-padding ,number? "The amount of padding to insert around
spanner bounds.")
     (bracket-flare ,number-pair? "A pair of numbers specifying how
much edges of brackets should slant outward.  Value @code{0.0} means
straight edges.")
     (bracket-visibility ,boolean-or-symbol? "This controls the
visibility of the tuplet bracket.  Setting it to false prevents
printing of the bracket.  Setting the property to @code{if-no-beam}
makes it print only if there is no beam associated with this tuplet
bracket.")
     (break-align-anchor ,number? "Grobs aligned to this break-align
grob will have their X-offsets shifted by this number.  In bar lines,
for example, this is used to position grobs relative to the (visual)
center of the bar line.")
     (break-align-anchor-alignment ,number? "Read by
@code{ly:break-aligned-interface::calc-extent-aligned-anchor} for
aligning an anchor to a grob's extent.")
     (break-align-orders ,vector? "Defines the order in which
prefatory matter (clefs, key signatures) appears.  The format is a
vector of length@tie{}3, where each element is one order for
end-of-line, middle of line, and start-of-line, respectively.  An
order is a list of symbols.

For example, clefs are put after key signatures by setting

@example
\\override Score.BreakAlignment #'break-align-orders =
  #(make-vector 3 '(span-bar
                    breathing-sign
                    staff-bar
                    key
                    clef
                    time-signature))
@end example")
     (break-align-symbol ,symbol? "This key is used for aligning and
spacing breakable items.")
     (break-align-symbols ,list? "A list of symbols that determine
which break-aligned grobs to align this to.  If the grob selected by
the first symbol in the list is invisible due to break-visibility, we
will align to the next grob (and so on).  Choices are @code{left-edge},
@code{ambitus}, @code{breathing-sign}, @code{clef}, @code{staff-bar},
@code{key-cancellation}, @code{key-signature}, @code{time-signature},
and @code{custos}.")
     (break-overshoot ,number-pair? "How much does a broken spanner
stick out of its bounds?")
     (break-visibility ,vector? "A vector of 3@tie{}booleans,
@code{#(@var{end-of-line} @var{unbroken} @var{begin-of-line})}.
@code{#t} means visible, @code{#f} means killed.")
     (breakable ,boolean? "Allow breaks here.")
     (broken-bound-padding ,number? "The amount of padding to insert
when a spanner is broken at a line break.")

;;;
;;; c
;;;
     (circled-tip ,boolean? "Put a circle at start/@/end of
hairpins (al/@/del niente).")
     (clip-edges ,boolean? "Allow outward pointing beamlets at the
edges of beams?")
     (collapse-height ,ly:dimension? "Minimum height of system start
delimiter.  If equal or smaller, the bracket/@/brace/@/line is removed.")
     (collision-bias ,number? "Number determining how much to favor the
left (negative) or right (positive).  Larger absolute values in either
direction will push a collision in this direction.")
     (collision-interfaces ,list? "A list of interfaces for which
automatic beam-collision resolution is run.")
     (collision-padding ,number? "Amount of padding to apply after
a collision is detected via the self-alignment-interface.")
     (collision-voice-only ,boolean? "Does automatic beam collsion apply
only to the voice in which the beam was created?")
     (color ,color? "The color of this grob.")
     (common-shortest-duration ,ly:moment? "The most common shortest
note length.  This is used in spacing.  Enlarging this sets the score
tighter.")
     (concaveness ,number? "A beam is concave if its inner stems are
closer to the beam than the two outside stems.  This number is a
measure of the closeness of the inner stems.  It is used for damping
the slope of the beam.")
     (connect-to-neighbor ,pair? "Pair of booleans, indicating whether
this grob looks as a continued break.")
     (control-points ,list? "List of offsets (number pairs) that form
control points for the tie, slur, or bracket shape.  For B@'eziers,
this should list the control points of a third-order B@'ezier curve.")
     (count-from ,integer? "The first measure in a measure count
receives this number.  The following measures are numbered in
increments from this initial value.")

;;;
;;; d
;;;
     (damping ,number? "Amount of beam slope damping.")
     (dash-definition ,pair? "List of @code{dash-elements} defining the
dash structure.  Each @code{dash-element} has a starting t value,
an ending t-value, a @code{dash-fraction}, and a @code{dash-period}.")
     (dash-fraction ,number? "Size of the dashes, relative to
@code{dash-period}.  Should be between @code{0.0} (no line) and
@code{1.0} (continuous line).")
     (dash-period ,number? "The length of one dash together with
whitespace.  If negative, no line is drawn at all.")
     (default-direction ,ly:dir? "Direction determined by note head
positions.")
     (default-staff-staff-spacing ,list? "The settings to use for
@code{staff-staff-spacing} when it is unset, for ungrouped staves
and for grouped staves that do not have the relevant
@code{StaffGrouper} property set (@code{staff-staff-spacing} or
@code{staffgroup-staff-spacing}).")
     (details ,list? "Alist of parameters for detailed grob behavior.
More information on the allowed parameters for a grob can be found by
looking at the top of the Internals Reference page for each interface
having a @code{details} property.")
     (digit-names ,vector? "Names for string finger digits.")
     (direction ,ly:dir? "If @code{side-axis} is @code{0} (or
@code{X}), then this property determines whether the object is placed
@code{LEFT}, @code{CENTER} or @code{RIGHT} with respect to the
other object.  Otherwise, it determines whether the object is placed
@code{UP}, @code{CENTER} or @code{DOWN}.  Numerical values may also
be used: @code{UP}=@code{1}, @code{DOWN}=@w{@code{-1}},
@code{LEFT}=@w{@code{-1}}, @code{RIGHT}=@code{1},
@code{CENTER}=@code{0}.")
     (dot-count ,integer? "The number of dots.")
     (dot-negative-kern ,number? "The space to remove between a dot
and a slash in percent repeat glyphs.  Larger values bring the two
elements closer together.")
     (dot-placement-list ,list? "List consisting of
@code{(@var{description} @var{string-number} @var{fret-number}
@var{finger-number})} entries used to define fret diagrams.")
     (duration-log ,integer? "The 2-log of the note head duration,
i.e., @code{0} = whole note, @code{1} = half note, etc.")


;;;
;;; e
;;;
     (eccentricity ,number? "How asymmetrical to make a slur.
Positive means move the center to the right.")
     (edge-height ,pair? "A pair of numbers specifying the heights of
the vertical edges: @code{(@var{left-height} . @var{right-height})}.")
     (edge-text ,pair? "A pair specifying the texts to be set at the
edges: @code{(@var{left-text} . @var{right-text})}.")
     (round-up-exceptions ,list? "A list of pairs where car is the numerator
and cdr the denominator of a moment.  Each pair in this list means that
the multi-measure rests of the corresponding length will be rounded up to
the longer rest.  See @var{round-up-to-longer-rest}.")
     (expand-limit ,integer? "Maximum number of measures expanded in
church rests.")
     ;; remove me?
     (extra-dy ,number? "Slope glissandi this much extra.")
     (extra-offset ,number-pair? "A pair representing an offset.  This
offset is added just before outputting the symbol, so the typesetting
engine is completely oblivious to it.  The values are measured in
@code{staff-space} units of the staff's @code{StaffSymbol}.")
     (extra-spacing-height ,number-pair? "In the horizontal spacing
problem, we increase the height of each item by this amount (by adding
the @q{car} to the bottom of the item and adding the @q{cdr} to the
top of the item).  In order to make a grob infinitely high (to prevent
the horizontal spacing problem from placing any other grobs above or
below this grob), set this to @code{(-inf.0 . +inf.0)}.")
     (extra-spacing-width ,number-pair? "In the horizontal spacing
problem, we pad each item by this amount (by adding the @q{car} on the
left side of the item and adding the @q{cdr} on the right side of the
item).  In order to make a grob take up no horizontal space at all,
set this to @code{(+inf.0 . -inf.0)}.")


;;;
;;; f
;;;
     (flag-count ,number? "The number of tremolo beams.")
     (flat-positions ,list? "Flats in key signatures are placed
within the specified ranges of staff-positions.  The general form
is a list of pairs, with one pair for each type of clef, in order
of the staff-position at which each clef places C:
@code{(alto treble tenor soprano baritone mezzosoprano bass)}.
If the list contains a single element it applies for all clefs.
A single number in place of a pair sets accidentals within the octave
ending at that staff-position.")
     (font-encoding ,symbol? "The font encoding is the broadest
category for selecting a font.  Currently, only lilypond's system
fonts (Emmentaler) are using this property.  Available
values are @code{fetaMusic} (Emmentaler), @code{fetaBraces},
@code{fetaText} (Emmentaler).")
     (font-family ,symbol? "The font family is the broadest category
for selecting text fonts.  Options include: @code{sans},
@code{roman}.")
     (font-name ,string? "Specifies a file name (without extension) of
the font to load.  This setting overrides selection using
@code{font-family}, @code{font-series} and @code{font-shape}.")
     (font-series ,symbol? "Select the series of a font.  Choices
include @code{medium}, @code{bold}, @code{bold-narrow}, etc.")
     (font-shape ,symbol? "Select the shape of a font.  Choices
include @code{upright}, @code{italic}, @code{caps}.")
     (font-size ,number? "The font size, compared to the
@q{normal}@tie{}size.  @code{0}@tie{}is style-sheet's normal size,
@w{@code{-1}} is smaller, @code{+1} is bigger.  Each step of@tie{}1 is
approximately 12% larger; 6@tie{}steps are exactly a factor@tie{}2
larger.  Fractional values are allowed.")
     (footnote ,boolean? "Should this be a footnote or in-note?")
     (footnote-music ,ly:music? "Music creating a footnote.")
     (footnote-text ,markup? "A footnote for the grob.")
     (force-hshift ,number? "This specifies a manual shift for notes
in collisions.  The unit is the note head width of the first voice
note.  This is used by @rinternals{note-collision-interface}.")
     (forced-spacing ,number? "Spacing forced between grobs, used in
various ligature engravers.")
     (fraction ,fraction? "Numerator and denominator of a time
signature object.")
     (french-beaming ,boolean? "Use French beaming style for this
stem.  The stem stops at the innermost beams.")
     (fret-diagram-details ,list? "An alist of detailed grob
properties for fret diagrams.  Each alist entry consists of a
@code{(@var{property} .  @var{value})} pair.  The properties which can
be included in @code{fret-diagram-details} include the following:

@itemize @bullet
@item
@code{barre-type} -- Type of barre indication used.  Choices include
@code{curved}, @code{straight}, and @code{none}.  Default
@code{curved}.
@item
@code{capo-thickness} -- Thickness of capo indicator, in multiples of
fret-space.  Default value@tie{}0.5.
@item
@code{dot-color} -- Color of dots.  Options include @code{black} and
@code{white}.  Default @code{black}.
@item
@code{dot-label-font-mag} -- Magnification for font used to label fret
dots.  Default value@tie{}1.
@item
@code{dot-position} -- Location of dot in fret space.  Default 0.6 for
dots without labels, 0.95-@code{dot-radius} for dots with labels.
@item
@code{dot-radius} -- Radius of dots, in terms of fret spaces.  Default
value 0.425 for labeled dots, 0.25 for unlabeled dots.
@item
@code{finger-code} -- Code for the type of fingering indication used.
Options include @code{none}, @code{in-dot}, and @code{below-string}.
Default @code{none} for markup fret diagrams, @code{below-string} for
@code{FretBoards} fret diagrams.
@item
@code{fret-count} -- The number of frets.  Default@tie{}4.
@item
@code{fret-label-custom-format} -- The format string to be used label
the lowest fret number, when @code{number-type} equals to
@code{custom}.  Default@tie{}\"~a\".
@item
@code{fret-label-font-mag} -- The magnification of the font used to
label the lowest fret number.  Default@tie{}0.5.
@item
@code{fret-label-vertical-offset} -- The offset of the fret label from
the center of the fret in direction parallel to strings.
Default@tie{}0.
@item
@code{label-dir} -- Side to which the fret label is attached.
@w{@code{-1}}, @code{LEFT}, or @code{DOWN} for left or down; @code{1},
@code{RIGHT}, or @code{UP} for right or up.  Default @code{RIGHT}.
@item
@code{mute-string} -- Character string to be used to indicate muted
string.  Default @code{\"x\"}.
@item
@code{number-type} -- Type of numbers to use in fret label.  Choices
include @code{roman-lower}, @code{roman-upper}, @code{arabic} and
@code{custom}.  In the later case, the format string is supplied by
the @code{fret-label-custom-format} property.
Default @code{roman-lower}.
@item
@code{open-string} -- Character string to be used to indicate open
string.  Default @code{\"o\"}.
@item
@code{orientation} -- Orientation of fret-diagram.  Options include
@code{normal}, @code{landscape}, and @code{opposing-landscape}.
Default @code{normal}.
@item
@code{string-count} -- The number of strings.  Default@tie{}6.
@item
@code{string-label-font-mag} -- The magnification of the font used to
label fingerings at the string, rather than in the dot.  Default value
0.6 for @code{normal} orientation, 0.5 for @code{landscape} and
@code{opposing-landscape}.
@item
@code{string-thickness-factor} -- Factor for changing thickness of
each string in the fret diagram.  Thickness of string @var{k} is given
by @code{thickness}
* (1+@code{string-thickness-factor})@tie{}^ (@var{k}-1).
Default@tie{}0.
@item
@code{top-fret-thickness} -- The thickness of the top fret line, as a
multiple of the standard thickness.  Default value@tie{}3.
@item
@code{xo-font-magnification} -- Magnification used for mute and open
string indicators.  Default value@tie{}0.5.
@item
@code{xo-padding} -- Padding for open and mute indicators from top
fret.  Default value 0.25.
@end itemize")
     ;; ugh: double, change.
     (full-length-padding ,number? "How much padding to use at the
right side of a full-length tuplet bracket.")
     (full-length-to-extent ,boolean? "Run to the extent of the column
for a full-length tuplet bracket.")
     (full-measure-extra-space ,number? "Extra space that is allocated
at the beginning of a measure with only one note.  This property is
read from the NonMusicalPaperColumn that begins the measure.")
     (full-size-change ,boolean? "Don't make a change clef smaller.")


;;;
;;; g
;;;
     (gap ,ly:dimension? "Size of a gap in a variable symbol.")
     (gap-count ,integer? "Number of gapped beams for tremolo.")
     (glissando-skip ,boolean? "Should this @code{NoteHead} be skipped
by glissandi?")
     (glyph ,string? "A string determining what @q{style} of glyph is
typeset.  Valid choices depend on the function that is reading this
property.

In combination with (span) bar lines, it is a string resembling the
bar line appearance in ASCII form.")
     (glyph-name ,string? "The glyph name within the font.

In the context of (span) bar lines, @var{glyph-name} represents
a processed form of @code{glyph}, where decisions about line breaking
etc. are already taken.")
     (glyph-name-alist ,list? "An alist of key-string pairs.")
     (graphical ,boolean? "Display in graphical (vs. text) form.")
     (grow-direction ,ly:dir? "Crescendo or decrescendo?")


;;;
;;; h
;;;
     (hair-thickness ,number? "Thickness of the thin line in a bar
line.")
     (harp-pedal-details ,list? "An alist of detailed grob properties
for harp pedal diagrams.  Each alist entry consists of a
@code{(@var{property} . @var{value})} pair.  The properties which can
be included in harp-pedal-details include the following:

@itemize @bullet
@item
@code{box-offset} -- Vertical shift of the center of flat/@/sharp pedal
boxes above/@/below the horizontal line.  Default value@tie{}0.8.
@item
@code{box-width} -- Width of each pedal box.  Default value@tie{}0.4.
@item
@code{box-height} -- Height of each pedal box.  Default value@tie{}1.0.
@item
@code{space-before-divider} -- Space between boxes before the first
divider (so that the diagram can be made symmetric).  Default
value@tie{}0.8.
@item
@code{space-after-divider} -- Space between boxes after the first
divider.  Default value@tie{}0.8.
@item
@code{circle-thickness} -- Thickness (in unit of the line-thickness)
of the ellipse around circled pedals.  Default value@tie{}0.5.
@item
@code{circle-x-padding} -- Padding in X direction of the ellipse
around circled pedals.  Default value 0.15.
@item
@code{circle-y-padding} -- Padding in Y direction of the ellipse
around circled pedals.  Default value@tie{}0.2.
@end itemize")
     (head-direction ,ly:dir? "Are the note heads left or right in a
semitie?")
     (height ,ly:dimension? "Height of an object in @code{staff-space}
units.")
     (height-limit ,ly:dimension? "Maximum slur height: The longer the
slur, the closer it is to this height.")
     (hide-tied-accidental-after-break ,boolean? "If set, an accidental
that appears on a tied note after a line break will not be displayed.")
     (horizon-padding ,number? "The amount to pad the axis
along which a @code{Skyline} is built for the
@code{side-position-interface}.")
     (horizontal-shift ,integer? "An integer that identifies ranking
of @code{NoteColumn}s for horizontal shifting.  This is used by
@rinternals{note-collision-interface}.")
     (horizontal-skylines ,ly:skyline-pair? "Two skylines, one to the
left and one to the right of this grob.")


;;;
;;; i
;;;
     (id ,string? "An id string for the grob.  Depending on the typestting
backend being used, this id will be assigned to a group containing all of
the stencils that comprise a given grob.  For example, in the svg backend,
the string will be assigned to the @code{id} attribute of a group (<g>)
that encloses the stencils that comprise the grob.  In the Postscript
backend, as there is no way to group items, the setting of the id property
will have no effect.")
     (ignore-collision ,boolean? "If set, don't do note collision
resolution on this @code{NoteColumn}.")
     (implicit ,boolean? "Is this an implicit bass figure?")
     (inspect-index ,integer? "If debugging is set, set beam and slur
configuration to this index, and print the respective scores.")
     (inspect-quants ,number-pair? "If debugging is set, set beam and
slur quants to this position, and print the respective scores.")


;;;
;;; k
;;;
     (keep-inside-line ,boolean? "If set, this column cannot have
objects sticking into the margin.")
     (kern ,ly:dimension? "Amount of extra white space to add.  For
bar lines, this is the amount of space after a thick line.")
     (knee ,boolean? "Is this beam kneed?")
     (knee-spacing-correction ,number? "Factor for the optical
correction amount for kneed beams.  Set between @code{0} for no
correction and @code{1} for full correction.")


;;;
;;; l
;;;
     (labels ,list? "List of labels (symbols) placed on a column.")
     (layer ,integer? "An integer which determines the order of printing
objects.  Objects with the lowest value of layer are drawn first, then
objects with progressively higher values are drawn, so objects with
higher values overwrite objects with lower values.  By default most
objects are assigned a layer value of 1.")
     (ledger-extra ,ly:dimension? "Extra distance from staff line to draw ledger
lines for.")
     (ledger-line-thickness ,number-pair? "The thickness of ledger
lines.  It is the sum of 2@tie{}numbers: The first is the factor for
line thickness, and the second for staff space.  Both contributions
are added.")
     (ledger-positions ,list? "Repeating pattern for the vertical positions
of ledger lines.  Bracketed groups are always shown together.")
     (left-bound-info ,list? "An alist of properties for determining
attachments of spanners to edges.")
     (left-padding ,ly:dimension? "The amount of space that is put
left to an object (e.g., a lyric extender).")
     (length ,ly:dimension? "User override for the stem length of
unbeamed stems.")
     (length-fraction ,number? "Multiplier for lengths.  Used for
determining ledger lines and stem lengths.")
     (line-break-penalty ,number? "Penalty for a line break at this
column.  This affects the choices of the line breaker; it avoids a
line break at a column with a positive penalty and prefers a line
break at a column with a negative penalty.")
     (line-break-permission ,symbol? "Instructs the line breaker on
whether to put a line break at this column.  Can be @code{force} or
@code{allow}.")
     (line-break-system-details ,list? "An alist of properties to use
if this column is the start of a system.")
     (line-count ,integer? "The number of staff lines.")
     (line-positions ,list? "Vertical positions of staff lines.")
     (line-thickness ,number? "The thickness of the tie or slur
contour.")
     (long-text ,markup? "Text markup.  See @ruser{Formatting text}.")


;;;
;;; m
;;;
     (max-beam-connect ,integer? "Maximum number of beams to connect
to beams from this stem.  Further beams are typeset as beamlets.")
     (max-stretch ,number? "The maximum amount that this
@code{VerticalAxisGroup} can be vertically stretched (for example, in
order to better fill a page).")
     (maximum-gap ,number? "Maximum value allowed for @code{gap}
property.")
     (measure-count ,integer? "The number of measures for a
multi-measure rest.")
     (measure-length ,ly:moment? "Length of a measure.  Used in some
spacing situations.")
     (merge-differently-dotted ,boolean? "Merge note heads in
collisions, even if they have a different number of dots.  This is
normal notation for some types of polyphonic music.

@code{merge-differently-dotted} only applies to opposing stem
directions (i.e., voice 1 &@tie{}2).")
     (merge-differently-headed ,boolean? "Merge note heads in
collisions, even if they have different note heads.  The smaller of
the two heads is rendered invisible.  This is used in polyphonic
guitar notation.  The value of this setting is used by
@rinternals{note-collision-interface}.

@code{merge-differently-headed} only applies to opposing stem
directions (i.e., voice 1 &@tie{}2).")
     (minimum-distance ,ly:dimension? "Minimum distance between rest
and notes or beam.")
     (minimum-length ,ly:dimension? "Try to make a spanner at least
this long, normally in the horizontal direction.  This requires an
appropriate callback for the @code{springs-and-rods} property.  If
added to a @code{Tie}, this sets the minimum distance between
noteheads.")
     (minimum-length-fraction ,number? "Minimum length of ledger line
as fraction of note head size.")
     (minimum-space ,ly:dimension? "Minimum distance that the victim
should move (after padding).")
     (minimum-X-extent ,number-pair? "Minimum size of an object in
X@tie{}dimension, measured in @code{staff-space} units.")
     (minimum-Y-extent ,number-pair? "Minimum size of an object in
Y@tie{}dimension, measured in @code{staff-space} units.")


;;;
;;; n
;;;
     (neutral-direction ,ly:dir? "Which direction to take in the
center of the staff.")
     (neutral-position ,number? "Position (in half staff spaces) where
to flip the direction of custos stem.")
     (next ,ly:grob? "Object that is next relation (e.g., the lyric
syllable following an extender).")
     (no-alignment ,boolean? "If set, don't place this grob in a
@code{VerticalAlignment}; rather, place it using its own
@code{Y-offset} callback.")
     (no-ledgers ,boolean? "If set, don't draw ledger lines on this
object.")
     (no-stem-extend ,boolean? "If set, notes with ledger lines do not
get stems extending to the middle staff line.")
     (non-break-align-symbols ,list? "A list of symbols that determine
which NON-break-aligned interfaces to align this to.")
     (non-default ,boolean? "Set for manually specified clefs.")
     (non-musical ,boolean? "True if the grob belongs to a
@code{NonMusicalPaperColumn}.")
     (nonstaff-nonstaff-spacing ,list? "The spacing alist
controlling the distance between the current non-staff line and
the next non-staff line in the direction of @code{staff-affinity},
if both are on the same side of the related staff, and
@code{staff-affinity} is either @code{UP} or @code{DOWN}.  See
@code{staff-staff-spacing} for a description of the alist
structure.")
     (nonstaff-relatedstaff-spacing ,list? "The spacing alist
controlling the distance between the current non-staff line and
the nearest staff in the direction of @code{staff-affinity}, if
there are no non-staff lines between the two, and
@code{staff-affinity} is either @code{UP} or @code{DOWN}.  If
@code{staff-affinity} is @code{CENTER}, then
@code{nonstaff-relatedstaff-spacing} is used for the nearest
staves on @emph{both} sides, even if other non-staff lines appear
between the current one and either of the staves.  See
@code{staff-staff-spacing} for a description of the alist
structure.")
     (nonstaff-unrelatedstaff-spacing ,list? "The spacing alist
controlling the distance between the current non-staff line and
the nearest staff in the opposite direction from
@code{staff-affinity}, if there are no other non-staff lines
between the two, and @code{staff-affinity} is either @code{UP} or
@code{DOWN}.  See @code{staff-staff-spacing} for a description of
the alist structure.")
     (normalized-endpoints ,pair? "Represents left and right placement
over the total spanner, where the width of the spanner is normalized
between 0 and 1.")
     (note-names ,vector? "Vector of strings containing names for
easy-notation note heads.")


;;;
;;; o
;;;
     (outside-staff-horizontal-padding ,number? "By default, an
outside-staff-object can be placed so that is it very close to another
grob horizontally.  If this property is set, the outside-staff-object
is raised so that it is not so close to its neighbor.")
     (outside-staff-padding ,number? "The padding to place between
grobs when spacing according to @code{outside-staff-priority}.
Two grobs with different @code{outside-staff-padding} values have
the larger value of padding between them.")
     (outside-staff-placement-directive ,symbol? "One of four directives
telling how outside staff objects should be placed.
@itemize @bullet
@item
@code{left-to-right-greedy} -- Place each successive grob from left to
right.
@item
@code{left-to-right-polite} -- Place a grob from left to right only if
it does not potentially overlap with another grob that has been placed
on a pass through a grob array. If there is overlap, do another pass to
determine placement.
@item
@code{right-to-left-greedy} -- Same as @code{left-to-right-greedy}, but
from right to left.
@item
@code{right-to-left-polite} -- Same as @code{left-to-right-polite}, but
from right to left.
@end itemize")
     (outside-staff-priority ,number? "If set, the grob is positioned
outside the staff in such a way as to avoid all collisions.  In case
of a potential collision, the grob with the smaller
@code{outside-staff-priority} is closer to the staff.")


;;;
;;; p
;;;
     (packed-spacing ,boolean? "If set, the notes are spaced as
tightly as possible.")
     (padding ,ly:dimension? "Add this much extra space between
objects that are next to each other.")
     (padding-pairs ,list? "An alist mapping @code{(@var{name}
. @var{name})} to distances.")
     (page-break-penalty ,number? "Penalty for page break at this
column.  This affects the choices of the page breaker; it avoids a
page break at a column with a positive penalty and prefers a page
break at a column with a negative penalty.")
     (page-break-permission ,symbol? "Instructs the page breaker on
whether to put a page break at this column.  Can be @code{force} or
@code{allow}.")
     (page-turn-penalty ,number? "Penalty for a page turn at this
column.  This affects the choices of the page breaker; it avoids a
page turn at a column with a positive penalty and prefers a page turn
at a column with a negative penalty.")
     (page-turn-permission ,symbol? "Instructs the page breaker on
whether to put a page turn at this column.  Can be @code{force} or
@code{allow}.")
     (parenthesized ,boolean? "Parenthesize this grob.")
     (positions ,number-pair? "Pair of staff coordinates
@code{(@var{left} . @var{right})}, where both @var{left} and
@var{right} are in @code{staff-space} units of the current staff.  For
slurs, this value selects which slur candidate to use; if extreme
positions are requested, the closest one is taken.")
     (prefer-dotted-right ,boolean? "For note collisions, prefer to
shift dotted up-note to the right, rather than shifting just the
dot.")
     (protrusion ,number? "In an arpeggio bracket, the length of the
horizontal edges.")

;;;
;;; r
;;;
     (ratio ,number? "Parameter for slur shape.  The higher this
number, the quicker the slur attains its @code{height-limit}.")
     (remove-empty ,boolean? "If set, remove group if it contains no
interesting items.")
     (remove-first ,boolean? "Remove the first staff of an orchestral
score?")
     (replacement-alist ,list? "Alist of strings.
The key is a string of the pattern to be replaced.  The value is a
string of what should be displayed.  Useful for ligatures.")
     (restore-first ,boolean? "Print a natural before the
accidental.")
     (rhythmic-location ,rhythmic-location? "Where (bar number,
measure position) in the score.")
     (right-bound-info ,list? "An alist of properties for determining
attachments of spanners to edges.")
     (right-padding ,ly:dimension? "Space to insert on the right side
of an object (e.g., between note and its accidentals).")
     (rotation ,list? "Number of degrees to rotate this object, and
what point to rotate around.  For example, @code{'(45 0 0)} rotates
by 45 degrees around the center of this object.")
     (rounded ,boolean? "Decide whether lines should be drawn rounded or not.")
     (round-up-to-longer-rest ,boolean? "Displays the longer multi-measure
rest when the length of a measure is between two values of
@code{usable-duration-logs}.  For example, displays a breve instead of a whole
in a 3/2 measure.")


;;;
;;; s
;;;
     (same-direction-correction ,number? "Optical correction amount
for stems that are placed in tight configurations.  This amount is
used for stems with the same direction to compensate for note head to
stem distance.")
     (script-priority ,number? "A key for determining the order of
scripts in a stack, by being added to the position of the script in
the user input, the sum being the overall priority.  Smaller means
closer to the head.")
     (self-alignment-X ,number? "Specify alignment of an object.  The
value @w{@code{-1}} means left aligned, @code{0}@tie{}centered, and
@code{1}@tie{}right-aligned in X@tie{}direction.  Other numerical
values may also be specified.")
     (self-alignment-Y ,number? "Like @code{self-alignment-X} but for
the Y@tie{}axis.")
     (sharp-positions ,list? "Sharps in key signatures are placed
within the specified ranges of staff-positions.  The general form
is a list of pairs, with one pair for each type of clef, in order
of the staff-position at which each clef places C:
@code{(alto treble tenor soprano baritone mezzosoprano bass)}.
If the list contains a single element it applies for all clefs.
A single number in place of a pair sets accidentals within the octave
ending at that staff-position.")
     (shorten-pair ,number-pair? "The lengths to shorten a
text-spanner on both sides, for example a pedal bracket.  Positive
values shorten the text-spanner, while negative values lengthen it.")
     (shortest-duration-space ,ly:dimension? "Start with this much
space for the shortest duration.  This is expressed in
@code{spacing-increment} as unit.  See also
@rinternals{spacing-spanner-interface}.")
     (shortest-playing-duration ,ly:moment? "The duration of the
shortest note playing here.")
     (shortest-starter-duration ,ly:moment? "The duration of the
shortest note that starts here.")
     (side-axis ,number? "If the value is @code{X} (or
equivalently@tie{}@code{0}), the object is placed horizontally next to
the other object.  If the value is @code{Y} or@tie{}@code{1}, it is
placed vertically.")
     (side-relative-direction ,ly:dir? "Multiply direction of
@code{direction-source} with this to get the direction of this
object.")
     (simple-Y ,boolean? "Should the Y placement of a spanner
disregard changes in system heights?")
     (size ,number? "Size of object, relative to standard size.")
     (skip-quanting ,boolean? "Should beam quanting be skipped?")
     (skyline-horizontal-padding ,number? "For determining the
vertical distance between two staves, it is possible to have a
configuration which would result in a tight interleaving of grobs from
the top staff and the bottom staff.  The larger this parameter is, the
farther apart the staves are placed in such a configuration.")
     (skyline-vertical-padding ,number? "The amount by which the left
and right skylines of a column are padded vertically, beyond the
@code{Y-extent}s and @code{extra-spacing-height}s of the constituent
grobs in the column.  Increase this to prevent interleaving of grobs
from adjacent columns.")
     (slash-negative-kern ,number? "The space to remove between
slashes in percent repeat glyphs.  Larger values bring the two
elements closer together.")
     (slope ,number? "The slope of this object.")
     (slur-padding ,number? "Extra distance between slur and script.")
     (snap-radius ,number? "The maximum distance between two objects that
will cause them to snap to alignment along an axis.")
     (space-alist ,list? "A table that specifies distances between
prefatory items, like clef and time-signature.  The format is an alist
of spacing tuples: @code{(@var{break-align-symbol} @var{type}
. @var{distance})}, where @var{type} can be the symbols
@code{minimum-space} or @code{extra-space}.")
     (space-to-barline ,boolean? "If set, the distance between a note
and the following non-musical column will be measured to the bar line
instead of to the beginning of the non-musical column.  If there is a
clef change followed by a bar line, for example, this means that we
will try to space the non-musical column as though the clef is not
there.")
     (spacing-increment ,number? "Add this much space for a doubled
duration.  Typically, the width of a note head.  See also
@rinternals{spacing-spanner-interface}.")
     (spacing-pair ,pair? "A pair of alignment symbols which set an object's
spacing relative to its left and right @code{BreakAlignment}s.

For example, a @code{MultiMeasureRest} will ignore prefatory items at its
bounds (i.e., clefs, key signatures and time signatures) using the following
override:

@example
\\override MultiMeasureRest
  #'spacing-pair = #'(staff-bar . staff-bar)
@end example")
     (spanner-id ,string? "An identifier to distinguish concurrent spanners.")
     (springs-and-rods ,boolean? "Dummy variable for triggering
spacing routines.")
     (stacking-dir ,ly:dir? "Stack objects in which direction?")
     (staff-affinity ,ly:dir? "The direction of the staff to use
for spacing the current non-staff line.  Choices are @code{UP},
@code{DOWN}, and @code{CENTER}.  If @code{CENTER}, the non-staff
line will be placed equidistant between the two nearest staves on
either side, unless collisions or other spacing constraints
prevent this.  Setting @code{staff-affinity} for a staff causes it
to be treated as a non-staff line.  Setting @code{staff-affinity}
to @code{#f} causes a non-staff line to be treated as a staff.")
     (staff-padding ,ly:dimension? "Maintain this much space between
reference points and the staff.  Its effect is to align objects of
differing sizes (like the dynamics @b{p} and @b{f}) on their
baselines.")
     (staff-position ,number? "Vertical position, measured in half
staff spaces, counted from the middle line.")
     (staff-space ,ly:dimension? "Amount of space between staff lines,
expressed in global @code{staff-space}.")
     (staff-staff-spacing ,list? "When applied to a staff-group's
@code{StaffGrouper} grob, this spacing alist controls the distance
between consecutive staves within the staff-group.  When applied
to a staff's @code{VerticalAxisGroup} grob, it controls the
distance between the staff and the nearest staff below it in the
same system, replacing any settings inherited from the
@code{StaffGrouper} grob of the containing staff-group, if there
is one.  This property remains in effect even when non-staff lines
appear between staves.  The alist can contain the following keys:

@itemize

@item
@code{basic-distance} -- the vertical distance, measured in
staff-spaces, between the reference points of the two items when
no collisions would result, and no stretching or compressing is in
effect.

@item
@code{minimum-distance} -- the smallest allowable vertical
distance, measured in staff-spaces, between the reference points
of the two items, when compressing is in effect.

@item
@code{padding} -- the minimum required amount of unobstructed
vertical whitespace between the bounding boxes (or skylines) of
the two items, measured in staff-spaces.

@item
@code{stretchability} -- a unitless measure of the dimension's
relative propensity to stretch.  If zero, the distance will not
stretch (unless collisions would result).

@end itemize")
     (staffgroup-staff-spacing ,list? "The spacing alist
controlling the distance between the last staff of the current
staff-group and the staff just below it in the same system, even
if one or more non-staff lines exist between the two staves.  If
the @code{staff-staff-spacing} property of the staff's
@code{VerticalAxisGroup} grob is set, that is used instead.  See
@code{staff-staff-spacing} for a description of the alist
structure.")
     (stem-attachment ,number-pair? "An @code{(@var{x} . @var{y})}
pair where the stem attaches to the notehead.")
     (stem-begin-position ,number? "User override for the
begin position of a stem.")
     ;;[TODO: doco]
     (stem-spacing-correction ,number? "Optical correction amount for
stems that are placed in tight configurations.  For opposite
directions, this amount is the correction for two normal sized stems
that overlap completely.")
     (stemlet-length ,number? "How long should be a stem over a
rest?")
     (stencil ,ly:stencil? "The symbol to print.")
     (stencils ,list? "Multiple stencils, used as intermediate
value.")
     (strict-grace-spacing ,boolean? "If set, main notes are spaced
normally, then grace notes are put left of the musical columns for the
main notes.")
     (strict-note-spacing ,boolean? "If set, unbroken columns with
non-musical material (clefs, bar lines, etc.) are not spaced
separately, but put before musical columns.")
     (stroke-style ,string? "Set to @code{\"grace\"} to turn stroke
through flag on.")
     (style ,symbol? "This setting determines in what style a grob is
typeset.  Valid choices depend on the @code{stencil} callback reading
this property.")


;;;
;;; t
;;;
     (text ,markup? "Text markup.  See @ruser{Formatting text}.")
     ;;FIXME -- Should both be the same?
     (text-direction ,ly:dir? "This controls the ordering of the
words.  The default @code{RIGHT} is for roman text.  Arabic or Hebrew
should use @code{LEFT}.")
     (thick-thickness ,number? "Bar line thickness, measured in
@code{line-thickness}.")
     (thickness ,number? "Line thickness, generally measured in
@code{line-thickness}.")
     (thin-kern ,number? "The space after a hair-line in a bar line.")
     (tie-configuration ,list? "List of @code{(@var{position} .
@var{dir})} pairs, indicating the desired tie configuration, where
@var{position} is the offset from the center of the staff in staff
space and @var{dir} indicates the direction of the tie (@code{1}=>up,
@w{@code{-1}}=>down, @code{0}=>center).  A non-pair entry
in the list causes the corresponding tie to be formatted
automatically.")
     (to-barline ,boolean? "If true, the spanner will stop at the bar
line just before it would otherwise stop.")
     (toward-stem-shift ,number? "Amount by which scripts are shifted
toward the stem if their direction coincides with the stem direction.
@code{0.0} means keep the default position (centered on the note
head), @code{1.0} means centered on the stem.  Interpolated values are
possible.")
     (transparent ,boolean? "This makes the grob invisible.")


;;;
;;; u
;;;
     (uniform-stretching ,boolean? "If set, items stretch
proportionally to their durations.  This looks better in complex
polyphonic patterns.")
     (used ,boolean? "If set, this spacing column is kept in the
spacing problem.")
     (usable-duration-logs ,list? "List of @code{duration-log}s that
can be used in typesetting the grob.")
     (use-skylines ,boolean? "Should skylines be used for side
positioning?")

;;;
;;; v
;;;
     (vertical-skylines ,ly:skyline-pair? "Two skylines, one above and
one below this grob.")

;;;
;;; w
;;;
     (when ,ly:moment? "Global time step associated with this column
happen?")
     (whiteout ,boolean? "If true, the grob is printed over a white
background to white-out underlying material, if the grob is visible.
 Usually #f by default.")
     (width ,ly:dimension? "The width of a grob measured in staff
space.")
     (word-space ,ly:dimension? "Space to insert between words in
texts.")


;;;
;;; x
;;;
     (X-extent ,number-pair? "Extent (size) in the X@tie{}direction,
measured in staff-space units, relative to object's reference point.")
     (X-offset ,number? "The horizontal amount that this object is
moved relative to its X-parent.")
     (X-positions ,number-pair? "Pair of X staff coordinates of a spanner
in the form @code{(@var{left} . @var{right})}, where both @var{left} and
@var{right} are in @code{staff-space} units of the current staff.")


;;;
;;; y
;;;
     (Y-extent ,number-pair? "Extent (size) in the Y@tie{}direction,
measured in staff-space units, relative to object's reference point.")
     (Y-offset ,number? "The vertical amount that this object is moved
relative to its Y-parent.")

;;;
;;; z
;;;
     (zigzag-length ,ly:dimension? "The length of the lines of a
zigzag, relative to @code{zigzag-width}.  A value of@tie{}@code{1}
gives 60-degree zigzags.")
     (zigzag-width ,ly:dimension? "The width of one zigzag squiggle.
This number is adjusted slightly so that the glissando line can be
constructed from a whole number of squiggles.")
     )))


;;;;;;;;;;;;;;;;
;;   INTERNAL


(define (define-internal-grob-property symbol type? description)
  (define-grob-property symbol type? description)
  (set-object-property! symbol 'backend-internal #t)
  symbol
  )


(define-public all-internal-grob-properties
  (map
   (lambda (x)
     (apply define-internal-grob-property x))

   `(
     ;;;;;;;;;;;;;;;;
     ;; grobs & grob arrays.  (alphabetical)
     (accidental-grob ,ly:grob? "The accidental for this note.")
     (accidental-grobs ,list? "An alist with @code{(@var{notename} .
@var{groblist})} entries.")
     (adjacent-spanners ,ly:grob-array? "An array of directly neighboring
dynamic spanners.")
     (all-elements ,ly:grob-array? "An array of all grobs in this line.  Its
function is to protect objects from being garbage collected.")
     (axis-group-parent-X ,ly:grob? "Containing X@tie{}axis group.")
     (axis-group-parent-Y ,ly:grob? "Containing Y@tie{}axis group.")

     (bars ,ly:grob-array? "An array of bar line pointers.")
     (beam ,ly:grob? "A pointer to the beam, if applicable.")
     (beam-segments ,list? "Internal representation of beam segments.")
     (bound-alignment-interfaces ,list "Interfaces to be used
for positioning elements that align with a column.")
     (bounded-by-me ,ly:grob-array? "An array of spanners that have this
column as start/@/begin point.  Only columns that have grobs or act as
bounds are spaced.")
     (bracket ,ly:grob? "The bracket for a number.")

     (c0-position ,integer? "An integer indicating the position of
middle@tie{}C.")
     (columns ,ly:grob-array? "An array of grobs, typically containing
@code{PaperColumn} or @code{NoteColumn} objects.")
     (concurrent-hairpins ,ly:grob-array? "All concurrent hairpins.")
     (conditional-elements ,ly:grob-array? "Internal use only.")
     (covered-grobs ,ly:grob-array? "Grobs that could potentially collide
with a beam.")

     (direction-source ,ly:grob? "In case @code{side-relative-direction} is
set, which grob to get the direction from.")
     (display-cautionary ,boolean? "Should the grob be displayed as a cautionary grob?")
     (dot ,ly:grob? "A reference to a @code{Dots} object.")
     (dots ,ly:grob-array? "Multiple @code{Dots} objects.")

     (elements ,ly:grob-array? "An array of grobs; the type is depending on
the grob where this is set in.")
     (encompass-objects ,ly:grob-array? "Objects that a slur should avoid
in addition to notes and stems.")

     (figures ,ly:grob-array? "Figured bass objects for continuation line.")
     (flag ,ly:grob? "A pointer to a @code{Flag} object.")
     (footnote-stencil ,ly:stencil? "The stencil of a system's footnotes.")
     (footnotes-before-line-breaking ,ly:grob-array? "Footnote grobs of
a whole system.")
     (footnotes-after-line-breaking ,ly:grob-array? "Footnote grobs of
a broken system.")

     (glissando-index ,integer? "The index of a glissando in its note
column.")
     (grace-spacing ,ly:grob? "A run of grace notes.")

     (has-span-bar ,pair? "A pair of grobs containing the span bars to
be drawn below and above the staff.  If no span bar is in a position,
the respective element is set to @code{#f}.")
     (heads ,ly:grob-array? "An array of note heads.")

     (items-worth-living ,ly:grob-array? "An array of interesting items.  If
empty in a particular staff, then that staff is erased.")
     (in-note-direction ,ly:dir? "Direction to place in-notes above a system.")
     (in-note-padding ,number? "Padding between in-notes.")
     (in-note-stencil ,ly:stencil? "The stencil of a system's in-notes.")

     (keep-alive-with ,ly:grob-array? "An array of other
@code{VerticalAxisGroup}s.  If any of them are alive, then we will stay alive.")

     (left-items ,ly:grob-array? "DOCME")
     (left-neighbor ,ly:grob? "The right-most column that has a spacing-wish
for this column.")

     (melody-spanner ,ly:grob? "The @code{MelodyItem} object for a stem.")
     (minimum-translations-alist ,list? "An list of translations for a given
start and end point.")

     (neighbors ,ly:grob-array? "The X-axis neighbors of a grob. Used by the
pure-from-neighbor-interface to determine various grob heights.")

     (normal-stems ,ly:grob-array? "An array of visible stems.")
     (note-columns ,ly:grob-array? "An array of @code{NoteColumn} grobs.")
     (note-head ,ly:grob? "A single note head.")
     (note-heads ,ly:grob-array? "An array of note head grobs.")
     (pedal-text ,ly:grob? "A pointer to the text of a mixed-style piano
pedal.")
     (potential-X-colliding-grobs ,ly:grob-array? "Grobs that can potentially
collide with a self-aligned grob on the X-axis.")
     (pure-relevant-grobs ,ly:grob-array? "All the grobs (items and spanners)
that are relevant for finding the @code{pure-Y-extent}")
     (pure-relevant-items ,ly:grob-array? "A subset of elements that are
relevant for finding the @code{pure-Y-extent}.")
     (pure-relevant-spanners ,ly:grob-array? "A subset of elements that are
relevant for finding the @code{pure-Y-extent}.")
     (pure-Y-common ,ly:grob? "A cache of the
@code{common_refpoint_of_array} of the @code{elements} grob set.")

     (rest ,ly:grob? "A pointer to a @code{Rest} object.")
     (rest-collision ,ly:grob? "A rest collision that a rest is in.")
     (rests ,ly:grob-array? "An array of rest objects.")
     (right-items ,ly:grob-array? "DOCME")
     (right-neighbor ,ly:grob? "See @code{left-neighbor}.")

     (side-support-elements ,ly:grob-array? "The side support, an array of
grobs.")
     (slur ,ly:grob? "A pointer to a @code{Slur} object.")
     (spacing ,ly:grob? "The spacing spanner governing this section.")
     (spacing-wishes ,ly:grob-array? "An array of note spacing or staff spacing
objects.")
     (span-start ,boolean? "Is the note head at the start of a spanner?")
     (spanner-broken ,boolean? "Indicates whether spanner
alignment should be broken after the current spanner.")
     (spanner-placement ,ly:dir? "The place of an annotation on a spanner.
@code{LEFT} is for the first spanner, and @code{RIGHT} is for the last.
@code{CENTER} will place it on the broken spanner that falls closest to the
center of the length of the entire spanner, although this behavior is
unpredictable in situations with lots of rhythmic diversity.  For predictable
results, use @code{LEFT} and @code{RIGHT}.")
     (staff-grouper ,ly:grob? "The staff grouper we belong to.")
     (staff-symbol ,ly:grob? "The staff symbol grob that we are in.")
     (stem ,ly:grob? "A pointer to a @code{Stem} object.")
     (stems ,ly:grob-array? "An array of stem objects.")

     (tie ,ly:grob? "A pointer to a @code{Tie} object.")
     (ties ,ly:grob-array? "A grob array of @code{Tie} objects.")
     (tremolo-flag ,ly:grob? "The tremolo object on a stem.")
     (tuplet-number ,ly:grob? "The number for a bracket.")
     (tuplet-start ,boolean? "Is stem at the start of a tuplet?")
     (tuplets ,ly:grob-array? "An array of smaller tuplet brackets.")

     (vertical-alignment ,ly:grob? "The VerticalAlignment in a System.")
     (vertical-skyline-elements ,ly:grob-array? "An array of grobs
used to create vertical skylines.")

     (X-colliding-grobs ,ly:grob-array? "Grobs that can collide
with a self-aligned grob on the X-axis.")
     (Y-colliding-grobs ,ly:grob-array? "Grobs that can collide
with a self-aligned grob on the Y-axis.")
     (X-common ,ly:grob? "Common reference point for axis group.")
     (Y-common ,ly:grob? "See @code{X-common}.")

     ;;;;;;;;;;;;;;;;
     ;; other
     (adjacent-pure-heights ,pair? "A pair of vectors.  Used by a
@code{VerticalAxisGroup} to cache the @code{Y-extent}s of different column
ranges.")

     (begin-of-line-visible ,boolean? "Set to make @code{ChordName} or
@code{FretBoard} be visible only at beginning of line or at
chord changes.")

     (cause ,scheme? "Any kind of causation objects (i.e., music, or perhaps
translator) that was the cause for this grob.")
     (cross-staff ,boolean? "True for grobs whose @code{Y-extent} depends on
inter-staff spacing.  The extent is measured relative to the grobs's parent
staff (more generally, its @code{VerticalAxisGroup}) so this boolean flags
grobs that are not rigidly fixed to their parent staff.
Beams that join notes from two staves are @code{cross-staff}.
Grobs that are positioned around such beams are also @code{cross-staff}.
Grobs that are grouping objects, however, like @code{VerticalAxisGroups}
will not in general be marked @code{cross-staff} when some of the members
of the group are @code{cross-staff}.")

     (delta-position ,number? "The vertical position difference.")

     (font ,ly:font-metric? "A cached font metric object.")
     (forced ,boolean? "Manually forced accidental.")

     (head-width ,ly:dimension? "The width of this ligature head.")

     (ideal-distances ,list? "@code{(@var{obj} . (@var{dist} .
@var{strength}))} pairs.")
     (important-column-ranks ,vector? "A cache of columns that contain
@code{items-worth-living} data.")
     (interfaces ,list? "A list of symbols indicating the interfaces
supported by this object.  It is initialized from the @code{meta} field.")

     (least-squares-dy ,number? "The ideal beam slope, without damping.")

     (maybe-loose ,boolean? "Used to mark a breakable column that is
loose if and only if it is in the middle of a line.")
     (meta ,list? "Provide meta information.  It is an alist with the
entries @code{name} and @code{interfaces}.")
     (minimum-distances ,list? "A list of rods that have the format
@code{(@var{obj} . @var{dist})}.")

     (note-collision ,ly:grob? "The @code{NoteCollision} object of a
dot column.")
     (numbering-assertion-function ,scheme? "The function used to assert
that footnotes are receiving correct automatic numbers.")

     (positioning-done ,boolean? "Used to signal that a positioning element
did its job.  This ensures that a positioning is only done once.")
     (pure-Y-extent ,number-pair? "The estimated height of a system.")
     (pure-Y-offset-in-progress ,boolean? "A debugging aid for catching
cyclic dependencies.")
     (quantize-position ,boolean? "If set, a vertical alignment is aligned
to be within staff spaces.")
     (quantized-positions ,number-pair? "The beam positions after
quanting.")


     (script-stencil ,pair? "A pair @code{(@var{type} . @var{arg})} which
acts as an index for looking up a @code{Stencil} object.")
     (shorten ,ly:dimension? "The amount of space that a stem is shortened.
Internally used to distribute beam shortening over stems.")
     (stem-info ,pair? "A cache of stem parameters.")
     (system-Y-offset ,number? "The Y-offset (relative to the bottom of the
top-margin of the page) of the system to which this staff belongs.")


     ;;;;;;;;;;;;;;;;
     ;; ancient notation

     ;;;;;;; TODO:
     ;; There are too many properties for ancient notation;
     ;; probably neume-types (a list of symbols) would also work.

     ;; However, such a list would consist of a couple of dozens of
     ;; entries, since head prefixes may be combined in many ways.  If
     ;; the macros in `gregorian.ly' would directly set prefix-set,
     ;; all the head prefixes could be junked; however, such macros
     ;; would be quite numerous, I guess.  --jr

     (add-cauda ,boolean? "Does this flexa require an additional cauda on
the left side?")
     (add-join ,boolean? "Is this ligature head-joined with the next one
by a vertical line?")
     (add-stem ,boolean? "Is this ligature head a virga and therefore needs
an additional stem on the right side?")
     (ascendens ,boolean? "Is this neume of ascending type?")
     (auctum ,boolean? "Is this neume liquescentically augmented?")

     (cavum ,boolean? "Is this neume outlined?")
     (context-info ,integer? "Within a ligature, the final glyph or shape of
a head may be affected by the left and/@/or right neighbour head.
@code{context-info} holds for each head such information about the left and
right neighbour, encoded as a bit mask.")

     (deminutum ,boolean? "Is this neume deminished?")
     (descendens ,boolean? "Is this neume of descendent type?")

     (flexa-height ,ly:dimension? "The height of a flexa shape in a ligature
grob (in @code{staff-space} units).")
     (flexa-interval ,integer? "The interval spanned by the two notes of a
flexa shape (1 is a second, 7 is an octave).")
     (flexa-width ,ly:dimension? "The width of a flexa shape in a
ligature grob in (in @code{staff-space} units).")
     (ligature-flexa ,boolean? "request joining note to the previous one
in a flexa.")

     (inclinatum ,boolean? "Is this neume an inclinatum?")


     (linea ,boolean? "Attach vertical lines to this neume?")

     (oriscus ,boolean? "Is this neume an oriscus?")

     (pes-or-flexa ,boolean? "Shall this neume be joined with the previous
head?")
     (prefix-set ,number? "A bit mask that holds all Gregorian head
prefixes, such as @code{\\virga} or @code{\\quilisma}.")
     (primitive ,integer? "A pointer to a ligature primitive, i.e., an item
similar to a note head that is part of a ligature.")

     (quilisma ,boolean? "Is this neume a quilisma?")

     (stropha ,boolean? "Is this neume a stropha?")

     (virga ,boolean? "Is this neume a virga?")

     (x-offset ,ly:dimension? "Extra horizontal offset for ligature heads.")

     )))

(define-public all-backend-properties
  (append
   all-internal-grob-properties
   all-user-grob-properties))
