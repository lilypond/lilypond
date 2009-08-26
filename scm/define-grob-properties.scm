;;;; define-grob-properties.scm -- part of generated backend documentation
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;;
;;;; (c) 1998--2009  Han-Wen Nienhuys <hanwen@xs4all.nl>
;;;;                 Jan Nieuwenhuizen <janneke@gnu.org>

(define (define-grob-property symbol type? description)
  (if (not (equal? (object-property symbol 'backend-doc) #f))
      (ly:error (_ "symbol ~S redefined") symbol))

  (set-object-property! symbol 'backend-type? type?)
  (set-object-property! symbol 'backend-doc description)
  symbol)

;; put this in an alist?
(define-public
  all-user-grob-properties

  (map
   (lambda (x)
     (apply define-grob-property x))

   `(
;;
;; a
;;
     (add-stem-support ,boolean? "If set, the @code{Stem} object is
included in this script's support.")
     (after-last-staff-spacing ,list? "An alist of spacing variables
that controls the spacing after the last staff in this staff group.
See @var{next-staff-spacing} for a description of the elements of
this alist.")
     (after-line-breaking ,boolean? "Dummy property, used to trigger
callback for @code{after-line-breaking}.")
     (align-dir ,ly:dir? "Which side to align? @code{-1}: left side,
@code{0}: around center of width, @code{1}: right side.")
     (allow-loose-spacing ,boolean? "If set, column can be detached
from main spacing.")
     (allow-span-bar ,boolean? "If false, no inter-staff bar line will
be created below this bar line.")
     (alteration ,number? "Alteration numbers for accidental.")
     (alteration-alist ,list? "List of @code{(@var{pitch}
. @var{accidental})} pairs for key signature.")
     (annotation ,string? "Annotate a grob for debug purposes.")
     (arpeggio-direction ,ly:dir? "If set, put an arrow on the
arpeggio squiggly line.")
     (arrow-length ,number? "Arrow length.")
     (arrow-width ,number? "Arrow width.")
     (auto-knee-gap ,ly:dimension? "If a gap is found between note
heads where a horizontal beam fits that is larger than this number,
make a kneed beam.")
     (average-spacing-wishes ,boolean? "If set, the spacing wishes are
averaged over staves.")
     (avoid-note-head ,boolean? "If set, the stem of a chord does not
pass through all note heads, but starts at the last note head.")
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


;;
;; b
;;
     (bar-size ,ly:dimension? "The size of a bar line.")
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
     (between-staff-spacing ,list? "An alist of spacing variables
that controls the spacing between staves within this staff group.
See @var{next-staff-spacing} for a description of the elements of
this alist.")
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


;;
;; c
;;
     (c0-position ,integer? "An integer indicating the position of
middle@tie{}C.")
     (circled-tip ,boolean? "Put a circle at start/end of
hairpins (al/del niente).")
     (clip-edges ,boolean? "Allow outward pointing beamlets at the
edges of beams?")
     (collapse-height ,ly:dimension? "Minimum height of system start
delimiter.  If equal or smaller, the bracket/brace/line is removed.")
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


;;
;; d
;;
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
     (default-next-staff-spacing ,list? "An alist of spacing variables
that controls the spacing between this staff and the next.
See @var{next-staff-spacing} for a description of the elements of
this alist.")
     (details ,list? "Alist of parameters for detailed grob behavior.
More information on the allowed parameters for a grob can be found by
looking at the top of the Internals Reference page for each interface
having a @code{details} property.")
     (digit-names ,vector? "Names for string finger digits.")
     (direction ,ly:dir? "If @code{side-axis} is @code{0} (or
@code{#X}), then this property determines whether the object is placed
@code{#LEFT}, @code{#CENTER} or @code{#RIGHT} with respect to the
other object.  Otherwise, it determines whether the object is placed
@code{#UP}, @code{#CENTER} or @code{#DOWN}.  Numerical values may also
be used: @code{#UP}=@code{1}, @code{#DOWN}=@code{-1},
@code{#LEFT}=@code{-1}, @code{#RIGHT}=@code{1},
@code{#CENTER}=@code{0}.")
     (dot-count ,integer? "The number of dots.")
     (dot-negative-kern ,number? "The space to remove between a dot
and a slash in percent repeat glyphs.  Larger values bring the two
elements closer together.")
     (dot-placement-list ,list? "List consisting of
@code{(@var{description} @var{string-number} @var{fret-number}
@var{finger-number})} entries used to define fret diagrams.")
     (duration-log ,integer? "The 2-log of the note head duration,
i.e., @code{0} = whole note, @code{1} = half note, etc.")


;;
;; e
;;
     (eccentricity ,number? "How asymmetrical to make a slur.
Positive means move the center to the right.")
     (edge-height ,pair? "A pair of numbers specifying the heights of
the vertical edges: @code{(@var{left-height} . @var{right-height})}.")
     (edge-text ,pair? "A pair specifying the texts to be set at the
edges: @code{(@var{left-text} . @var{right-text})}.")
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
     (extra-X-extent ,number-pair? "A grob is enlarged in
X@tie{}dimension by this much.")
     (extra-Y-extent ,number-pair? "A grob is enlarged in
Y@tie{}dimension by this much.")


;;
;; f
;;
     (flag ,ly:stencil? "A function returning the full flag stencil
for the @code{Stem}, which is passed to the function as the only
argument.  The default ly:stem::calc-stencil function uses the
@code{flag-style} property to determine the correct glyph for the
flag.  By providing your own function, you can create arbitrary
flags.")
     (flag-count ,number? "The number of tremolo beams.")
     (flag-style ,symbol? "A symbol determining what style of flag
glyph is typeset on a @code{Stem}.  Valid options include @code{'()}
for standard flags, @code{'mensural} and @code{'no-flag}, which
switches off the flag.")
     (font-encoding ,symbol? "The font encoding is the broadest
category for selecting a font.  Currently, only lilypond's system
fonts (Emmentaler and Aybabtu) are using this property.  Available
values are @code{fetaMusic} (Emmentaler), @code{fetaBraces} (Aybabtu),
@code{fetaNumber} (Emmentaler), and @code{fetaDynamic} (Emmentaler).")
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
@code{-1} is smaller, @code{+1} is bigger.  Each step of@tie{}1 is
approximately 12% larger; 6@tie{}steps are exactly a factor@tie{}2
larger.  Fractional values are allowed.")
     (force-hshift ,number? "This specifies a manual shift for notes
in collisions.  The unit is the note head width of the first voice
note.  This is used by @rinternals{note-collision-interface}.")
     (fraction ,number-pair? "Numerator and denominator of a time
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
@code{fret-label-font-mag} -- The magnification of the font used to
label the lowest fret number.  Default@tie{}0.5.
@item
@code{fret-label-vertical-offset} -- The offset of the fret label from
the center of the fret in direction parallel to strings.
Default@tie{}0.
@item
@code{label-dir} -- Side to which the fret label is attached.
@code{-1}, @code{#LEFT}, or @code{#DOWN} for left or down; @code{1},
@code{#RIGHT}, or @code{#UP} for right or up.  Default @code{#RIGHT}.
@item
@code{mute-string} -- Character string to be used to indicate muted
string.  Default @code{\"x\"}.
@item
@code{number-type} -- Type of numbers to use in fret label.  Choices
include @code{roman-lower}, @code{roman-upper}, and @code{arabic}.
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


;;
;; g
;;
     (gap ,ly:dimension? "Size of a gap in a variable symbol.")
     (gap-count ,integer? "Number of gapped beams for tremolo.")
     (glyph ,string? "A string determining what @q{style} of glyph is
typeset.  Valid choices depend on the function that is reading this
property.")
     (glyph-name ,string? "The glyph name within the font.")
     (glyph-name-alist ,list? "An alist of key-string pairs.")
     (grow-direction ,ly:dir? "Crescendo or decrescendo?")


;;
;; h
;;
     (hair-thickness ,number? "Thickness of the thin line in a bar
line.")
     (harp-pedal-details ,list? "An alist of detailed grob properties
for harp pedal diagrams.  Each alist entry consists of a
@code{(@var{property} . @var{value})} pair.  The properties which can
be included in harp-pedal-details include the following:

@itemize @bullet
@item
@code{box-offset} -- Vertical shift of the center of flat/sharp pedal
boxes above/below the horizontal line.  Default value@tie{}0.8.
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
     (horizontal-shift ,integer? "An integer that identifies ranking
of @code{NoteColumn}s for horizontal shifting.  This is used by
@rinternals{note-collision-interface}.")
     (horizontal-skylines ,ly:skyline-pair? "Two skylines, one to the
left and one to the right of this grob.")


;;
;; i
;;
     (ignore-collision ,boolean? "If set, don't do note collision
resolution on this @code{NoteColumn}.")
     (implicit ,boolean? "Is this an implicit bass figure?")
     (inspect-index ,integer? "If debugging is set, set beam and slur
configuration to this index, and print the respective scores.")
     (inspect-quants ,number-pair? "If debugging is set, set beam and
slur quants to this position, and print the respective scores.")
     (inter-loose-line-spacing ,list? "Specifies how to vertically
position a non-spaced line relative to the other non-spaced lines
around it.  See @var{next-staff-spacing} for the format of this list.")
     (inter-staff-spacing ,list? "Specifies how to vertically
position a non-spaced line relative to the staff for which it
has affinity.  See @var{next-staff-spacing} for the format of this list.")


;;
;; k
;;
     (keep-fixed-while-stretching ,boolean? "A grob with this property
set to true is fixed relative to the staff above it when systems are
stretched.")
     (keep-inside-line ,boolean? "If set, this column cannot have
objects sticking into the margin.")
     (kern ,ly:dimension? "Amount of extra white space to add.  For
bar lines, this is the amount of space after a thick line.")
     (knee ,boolean? "Is this beam kneed?")
     (knee-spacing-correction ,number? "Factor for the optical
correction amount for kneed beams.  Set between @code{0} for no
correction and @code{1} for full correction.")


;;
;; l
;;
     (labels ,list? "List of labels (symbols) placed on a column.")
     (layer ,integer? "The output layer (a value between 0
and@tie{}2): Layers define the order of printing objects.  Objects in
lower layers are overprinted by objects in higher layers.")
     (ledger-line-thickness ,number-pair? "The thickness of ledger
lines.  It is the sum of 2@tie{}numbers: The first is the factor for
line thickness, and the second for staff space.  Both contributions
are added.")
     (left-bound-info ,list? "An alist of properties for determining
attachments of spanners to edges.")
     (left-padding ,ly:dimension? "The amount of space that is put
left to an object (e.g., a group of accidentals).")
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


;;
;; m
;;
     (max-beam-connect ,integer? "Maximum number of beams to connect
to beams from this stem.  Further beams are typeset as beamlets.")
     (max-stretch ,number? "The maximum amount that this
@code{VerticalAxisGroup} can be vertically stretched (for example, in
order to better fill a page).")
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


;;
;; n
;;
     (neutral-direction ,ly:dir? "Which direction to take in the
center of the staff.")
     (neutral-position ,number? "Position (in half staff spaces) where
to flip the direction of custos stem.")
     (next ,ly:grob? "Object that is next relation (e.g., the lyric
syllable following an extender).")
     (next-staff-spacing ,list? "An alist of properties used to position
the next staff in the system.  The symbols that can be defined in the alist
are
@itemize @bullet
@item @var{space} -- the amount of stretchable space between the center
of this staff and the center of the next staff;
@item @var{padding} -- the minimum amount of whitespace that must be
present between this staff and the next staff;
@item @var{stretchability} -- the ease with which the stretchable
space increases when the system to which this staff belongs is stretched.
If this is zero, the distance to the next staff will be fixed either at
@var{space} or at @var{padding} plus the minimum distance to ensure
there is no overlap, whichever is larger;
@item @var{minimum-distance} -- the minimum distance to place between
the center of this staff and the center of the next. This differs
from @var{padding} in that the height of a staff has no effect on
the application of @var{minimum-distance} (whereas the height of a
staff is crucial for @var{padding}).
@end itemize")
     (no-alignment ,boolean? "If set, don't place this grob in a
@code{VerticalAlignment}; rather, place it using its own
@code{Y-offset} callback.")
     (no-ledgers ,boolean? "If set, don't draw ledger lines on this
object.")
     (no-stem-extend ,boolean? "If set, notes with ledger lines do not
get stems extending to the middle staff line.")
     (non-default ,boolean? "Set for manually specified clefs.")
     (non-musical ,boolean? "True if the grob belongs to a
@code{NonMusicalPaperColumn}.")
     (note-names ,vector? "Vector of strings containing names for
easy-notation note heads.")


;;
;; o
;;
     (outside-staff-horizontal-padding ,number? "By default, an
outside-staff-object can be placed so that is it very close to another
grob horizontally.  If this property is set, the outside-staff-object
is raised so that it is not so close to its neighbor.")
     (outside-staff-padding ,number? "The padding to place between
this grob and the staff when spacing according to
@code{outside-staff-priority}.")
     (outside-staff-priority ,number? "If set, the grob is positioned
outside the staff in such a way as to avoid all collisions.  In case
of a potential collision, the grob with the smaller
@code{outside-staff-priority} is closer to the staff.")


;;
;; p
;;
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


;;
;; r
;;
     (ratio ,number? "Parameter for slur shape.  The higher this
number, the quicker the slur attains its @code{height-limit}.")
     (remove-empty ,boolean? "If set, remove group if it contains no
interesting items.")
     (remove-first ,boolean? "Remove the first staff of an orchestral
score?")
     (restore-first ,boolean? "Print a natural before the
accidental.")
     (rhythmic-location ,rhythmic-location? "Where (bar number,
measure position) in the score.")
     (right-bound-info ,list? "An alist of properties for determining
attachments of spanners to edges.")
     (right-padding ,ly:dimension? "Space to insert on the right side
of an object (e.g., between note and its accidentals).")
     (rotation ,list? "Number of degrees to rotate this object, and
what point to rotate around.  For example, @code{#'(45 0 0)} rotates
by 45 degrees around the center of this object.")


;;
;; s
;;
     (same-direction-correction ,number? "Optical correction amount
for stems that are placed in tight configurations.  This amount is
used for stems with the same direction to compensate for note head to
stem distance.")
     (script-priority ,number? "A sorting key that determines in what
order a script is within a stack of scripts.")
     (self-alignment-X ,number? "Specify alignment of an object.  The
value @code{-1} means left aligned, @code{0}@tie{}centered, and
@code{1}@tie{}right-aligned in X@tie{}direction.  Other numerical
values may also be specified.")
     (self-alignment-Y ,number? "Like @code{self-alignment-X} but for
the Y@tie{}axis.")
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
     (side-axis ,number? "If the value is @code{#X} (or
equivalently@tie{}@code{0}), the object is placed horizontally next to
the other object.  If the value is @code{#Y} or@tie{}@code{1}, it is
placed vertically.")
     (side-relative-direction ,ly:dir? "Multiply direction of
@code{direction-source} with this to get the direction of this
object.")
     (size ,number? "Size of object, relative to standard size.")
     (skyline-horizontal-padding ,number? "For determining the
vertical distance between two staves, it is possible to have a
configuration which would result in a tight interleaving of grobs from
the top staff and the bottom staff.  The larger this parameter is, the
farther apart the staves are placed in such a configuration.")
     (slash-negative-kern ,number? "The space to remove between
slashes in percent repeat glyphs.  Larger values bring the two
elements closer together.")
     (slope ,number? "The slope of this object.")
     (slur-padding ,number? "Extra distance between slur and script.")
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
     (springs-and-rods ,boolean? "Dummy variable for triggering
spacing routines.")
     (stacking-dir ,ly:dir? "Stack objects in which direction?")
     (staff-affinity ,ly:dir? "The direction of the staff to which this
line should stick.")
     (staff-padding ,ly:dimension? "Maintain this much space between
reference points and the staff.  Its effect is to align objects of
differing sizes (like the dynamics @b{p} and @b{f}) on their
baselines.")
     (staff-position ,number? "Vertical position, measured in half
staff spaces, counted from the middle line.")
     (staff-space ,ly:dimension? "Amount of space between staff lines,
expressed in global @code{staff-space}.")
     (stem-attachment ,number-pair? "An @code{(@var{x} . @var{y})}
pair where the stem attaches to the notehead.")
     (stem-end-position ,number? "Where does the stem end (the end is
opposite to the support-head)?")
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


;;
;; t
;;
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
space and @var{dir} indicates the direction of the tie
(@code{1}=>up, @code{-1}=>down, @code{0}=>center).  A non-pair entry
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


;;
;; u
;;
     (uniform-stretching ,boolean? "If set, items stretch
proportionally to their durations.  This looks better in complex
polyphonic patterns.")
     (used ,boolean? "If set, this spacing column is kept in the
spacing problem.")


;;
;; v
;;
     (vertical-skylines ,ly:skyline-pair? "Two skylines, one above and
one below this grob.")


;;
;; w
;;
     (when ,ly:moment? "Global time step associated with this column
happen?")
     (width ,ly:dimension? "The width of a grob measured in staff
space.")
     (word-space ,ly:dimension? "Space to insert between words in
texts.")


;;
;; x
;;
     (X-extent ,number-pair? "Hard coded extent in X@tie{}direction.")
     (X-offset ,number? "The horizontal amount that this object is
moved relative to its X-parent.")


;;
;; y
;;
     (Y-extent ,number-pair? "Hard coded extent in Y@tie{}direction.")
     (Y-offset ,number? "The vertical amount that this object is moved
relative to its Y-parent.")

;;
;; z
;;
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
     (adjacent-hairpins ,ly:grob-array? "An array of directly neighboring
hairpins.")
     (all-elements ,ly:grob-array? "An array of all grobs in this line.  Its
function is to protect objects from being garbage collected.")
     (arpeggio ,ly:grob? "A pointer to an @code{Arpeggio} object.")
     (axis-group-parent-X ,ly:grob? "Containing X@tie{}axis group.")
     (axis-group-parent-Y ,ly:grob? "Containing Y@tie{}axis group.")

     (bars ,ly:grob-array? "An array of bar line pointers.")
     (beam ,ly:grob? "A pointer to the beam, if applicable.")
     (bounded-by-me ,ly:grob-array? "An array of spanners that have this
column as start/begin point.  Only columns that have grobs or act as
bounds are spaced.")
     (bracket ,ly:grob? "The bracket for a number.")

     (columns ,ly:grob-array? "An array of grobs, typically containing
@code{PaperColumn} or @code{NoteColumn} objects.")
     (conditional-elements ,ly:grob-array? "Internal use only.")

     (direction-source ,ly:grob? "In case @code{side-relative-direction} is
set, which grob to get the direction from.")
     (dot ,ly:grob? "A reference to a @code{Dots} object.")
     (dots ,ly:grob-array? "Multiple @code{Dots} objects.")

     (elements ,ly:grob-array? "An array of grobs; the type is depending on
the grob where this is set in.")
     (encompass-objects ,ly:grob-array? "Objects that a slur should avoid
in addition to notes and stems.")

     (figures ,ly:grob-array? "Figured bass objects for continuation line.")

     (grace-spacing ,ly:grob? "A run of grace notes.")

     (heads ,ly:grob-array? "An array of note heads.")

     (items-worth-living ,ly:grob-array? "An array of interesting items.  If
empty in a particular staff, then that staff is erased.")

     (left-items ,ly:grob-array? "DOCME")
     (left-neighbor ,ly:grob? "The right-most column that has a spacing-wish
for this column.")

     (normal-stems ,ly:grob-array? "An array of visible stems.")
     (note-columns ,ly:grob-array? "An array of @code{NoteColumn} grobs.")
     (note-head ,ly:grob? "A single note head.")
     (note-heads ,ly:grob-array? "An array of note head grobs.")
     (pedal-text ,ly:grob? "A pointer to the text of a mixed-style piano
pedal.")
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

     (separation-item ,ly:grob? "A pointer to a @code{SeparationItem}
object.")
     (side-support-elements ,ly:grob-array? "The side support, an array of
grobs.")
     (slur ,ly:grob? "A pointer to a @code{Slur} object.")
     (spaceable-staves ,ly:grob-array? "Objects to be spaced during page
layout.")
     (spacing ,ly:grob? "The spacing spanner governing this section.")
     (spacing-wishes ,ly:grob-array? "An array of note spacing or staff spacing
objects.")
     (staff-grouper ,ly:grob? "The staff grouper we belong to.")
     (staff-symbol ,ly:grob? "The staff symbol grob that we are in.")
     (stem ,ly:grob? "A pointer to a @code{Stem} object.")
     (stems ,ly:grob-array? "An array of stem objects.")

     (tie ,ly:grob? "A pointer to a @code{Tie} object.")
     (tremolo-flag ,ly:grob? "The tremolo object on a stem.")
     (tuplet-number ,ly:grob? "The number for a bracket.")
     (tuplets ,ly:grob-array? "An array of smaller tuplet brackets.")
     (X-common ,ly:grob? "Common reference point for axis group.")
     (Y-common ,ly:grob? "See @code{X-common}.")

     ;;;;;;;;;;;;;;;;
     ;; other
     (adjacent-pure-heights ,pair? "A pair of vectors.  Used by a
@code{VerticalAxisGroup} to cache the @code{Y-extent}s of different column
ranges.")
     
     (bar-extent ,number-pair? "The Y-extent of the actual bar line.
This may differ from @code{Y-extent} because it does not include the dots in
a repeat bar line.")
     (begin-of-line-visible ,boolean? "Set to make @code{ChordName} or
@code{FretBoard} be visible only at beginning of line or at
chord changes.")

     (cause ,scheme? "Any kind of causation objects (i.e., music, or perhaps
translator) that was the cause for this grob.")
     (cross-staff ,boolean? "For a beam or a stem, this is true if we
depend on inter-staff spacing.")

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

     (positioning-done ,boolean? "Used to signal that a positioning element
did its job.  This ensures that a positioning is only done once.")
     (pure-Y-extent ,number-pair? "The estimated height of a system.")
     (pure-Y-offset-in-progress ,boolean? "A debugging aid for catching
cyclic dependencies.")

     (quant-score ,string? "The beam quanting score; stored for
debugging.")
     (quantize-position ,boolean? "If set, a vertical alignment is aligned
to be within staff spaces.")
     (quantized-positions ,number-pair? "The beam positions after
quanting.")

     (script-stencil ,pair? "A pair @code{(@var{type} . @var{arg})} which
acts as an index for looking up a @code{Stencil} object.")
     (shorten ,ly:dimension? "The amount of space that a stem is shortened.
Internally used to distribute beam shortening over stems.")
     (skyline-distance ,number? "The distance between this staff and the
next one, as determined by a skyline algorithm.")
     (stem-info ,pair? "A cache of stem parameters.")
     (system-Y-offset ,number? "The Y-offset (relative to the bottom of the
top-margin of the page) of the system to which this staff belongs.")

     (use-breve-rest ,boolean? "Use breve rests for measures longer
than a whole rest.")

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
a head may be affected by the left and/or right neighbour head.
@code{context-info} holds for each head such information about the left and
right neighbour, encoded as a bit mask.")

     (deminutum ,boolean? "Is this neume deminished?")
     (descendens ,boolean? "Is this neume of descendent type?")

     (flexa-height ,ly:dimension? "The height of a flexa shape in a ligature
grob (in @code{staff-space} units).")
     (flexa-width ,ly:dimension? "The width of a flexa shape in a
ligature grob in (in @code{staff-space} units).")

     (inclinatum ,boolean? "Is this neume an inclinatum?")

     (join-right-amount ,number? "A length used for calculating the
Y-extent of mensural ligatures.")

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
