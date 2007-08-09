;;;; grob-property-description.scm -- part of generated backend documentation
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c) 1998--2007  Han-Wen Nienhuys <hanwen@xs4all.nl>
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
     (X-offset ,number? "The horizontal amount that this object is moved relative to its X-parent")
     (Y-offset ,number? "The vertical amount that this object is moved
relative to its Y-parent")
     (accidentals ,list? "List of alteration numbers")
     (after-line-breaking ,boolean? "Dummy property, used to trigger callback for after-line-breaking")
     (alteration-alist ,list? "List of @code{(@var{pitch}
. @var{accidental})} pairs for key signature.")
     (add-stem-support ,boolean? "If set, the Stem object is included in this script's support") 
     (align-dir ,ly:dir? "Which side to align? @code{-1}: left side,
@code{0}: around center of width, @code{1}: right side.")
     (allow-loose-spacing ,boolean? "If set, column can be detached from main spacing.")
     (arpeggio-direction ,ly:dir? "If set, put an
arrow on the arpeggio squiggly line.")
     (arrow ,boolean? "Add an arrow to the line.")
     (arrow-length ,number? "Arrow length.")
     (arrow-width ,number? "Arrow width.")
     (auto-knee-gap ,ly:dimension? "If a gap is found between note heads
where a horizontal beam fits that is larger than this number, make a kneed beam.")
     (average-spacing-wishes ,boolean? "If set, the spacing wishes are averaged over staves.")
     (avoid-note-head ,boolean? "If set, the stem of a chord does not
pass through all note heads, but starts at the last note head. ")
     (axes ,list? "list of axis numbers.
In the case of alignment grobs, this should contain only one number.")
     (bar-size ,ly:dimension? "size of a bar line.")
     (barre-type ,symbol? "Type of barre indication used in a fret diagram.
Choices include @code{curved} and @code{straight}.")
     (base-shortest-duration ,ly:moment?
			     "Spacing is based on the shortest notes in a piece. Normally, pieces are spaced as if notes at least as short as this are present.")
     (baseline-skip ,ly:dimension? "Distance between base lines of
 multiple lines of text.")
     (beam-thickness ,ly:dimension? "thickness, measured in staff-space.")
     (beam-width ,ly:dimension? "width of the tremolo sign.")
     (beamed-stem-shorten ,list? "How much to shorten beamed stems,
when their direction is forced. It is a list, since the value is different
depending on the number flags/beams.")
     (beaming ,pair?
	      "Pair of number lists. Each number list specifies which
beams to make. 0 is the central beam, 1 is the next beam toward the
note etc. This information is used to determine how to connect the
beaming patterns from stem to stem inside a beam.")
     (before-line-breaking ,boolean? "Dummy property, used to trigger a callback function.")
     (between-cols ,pair? "Where to attach a loose column to")
     (bound-padding ,number? "The amount of padding to insert around spanner bounds.")
     (bracket-flare ,number-pair? "A pair of numbers specifying how
much edges of brackets should slant outward.  Value 0.0 means straight
edges")
     (break-align-symbol ,symbol? "This key is used for aligning and
spacing breakable items.")
     (break-align-orders ,vector? " Defines the order in which
prefatory matter (clefs, key signatures) appears. The format is a
vector of length 3, where each element is one order for end-of-line,
middle of line, and start-of-line respectively. An order is a list of
symbols.

For example, clefs are put after key signatures by setting

@example
	\\override Score.BreakAlignment #'break-align-orders = #(make-vector  3
	  '(span-bar
	  breathing-sign
	  staff-bar
	  key
	  clef
	  time-signature))
@end example")
     (break-overshoot ,number-pair? "How much does a broken spanner
stick out of its bounds?")
     (bracket-visibility ,boolean-or-symbol? "This controls the
visibility of the tuplet bracket.  Setting it to false will prevent
printing of the bracket. Setting the property to @code{'if-no-beam}
will make it print only if there is no beam associated with this
tuplet bracket.")
     (break-visibility ,vector? "A vector of 3 booleans, #(end-of-line unbroken begin-of-line).
#t means visible, #f means killed.")
     (c0-position ,integer? "An integer indicating the position of
middle C.")
     (cautionary-style ,symbol? "How to print cautionary
accidentals. Choices are @code{smaller} or
@code{parentheses}.")
     (cautionary ,boolean? "Is this a cautionary accidental?")
     (concaveness ,number? "A beam is concave when its inner stems are
closer to the beam than the two outside stems. This number is a
measure of the closeness of the inner stems. It is used for damping
the slope of the beam.")
     (collapse-height ,ly:dimension? "Minimum height of system start delimiter.  If equal or smaller, the bracket is removed.")
     (color ,color? "The color of this grob.")
     (control-points ,list? "List of offsets (number-pairs) that form
control points for the tie/slur/bracket shape. For beziers, this
should list the control points of a 3rd order bezier curve." )
     (connect-to-neighbor ,pair? "Pair of booleans, indicating whether this
grob looks as a continued break.")
     (damping ,number? "Amount of beam slope damping. ")
     (dash-period ,number? "the length of one dash + white space. If
negative, no line is drawn at all.")
     (dash-fraction ,number? "Size of the dashes, relative to
dash-period. Should be between 0.0 (no line) and 1.0 (continuous
line).")
     (default-direction ,ly:dir? "Direction determined by note head positions.")
     (digit-names ,vector "Names for string finger digits. ")
     (direction ,ly:dir? "If side-position is 1 (#X), then this property determines if the object is placed #LEFT, #CENTER or #RIGHT with respect to the other object. Otherwise, it determines if the object is placed #UP #CENTER or #DOWN.  Numerical values may also be used. #UP=1, #DOWN=-1, #LEFT=-1, #RIGHT=1, CENTER=0 but also other numerical values are permitted.")
     (dot-color ,symbol? "Color of dots.  Options include 
@code{black} and @code{white}.")
     (dot-radius ,number? "Radius of dots.")
     (dot-count ,integer? "The number of dots.")
     (duration-log ,integer? "The 2-log of the note head duration, i.e. 0=whole note, 1 = half note, etc.")
     (edge-height ,pair? "A pair of number specifying the heights of
the vertical edges '(@var{left-height} . @var{right-height}).")
     (edge-text ,pair? "A pair specifying the texts to be set at the
edges '(@var{left-text} . @var{right-text}).")
     (eccentricity ,number? "How asymmetrical to make a slur. Positive means move the center to the right.")
     (enclose-bounds ,number?
		     "How much of the bound a spanner should enclose:
+1 = completely, 0 = center, -1 not at all.")
     (expand-limit ,integer? "maximum number of measures expanded in church rests.")
     ;; remove me? 
     (extra-X-extent ,number-pair? "A grob is enlarged in X dimension
by this much.")
     (extra-Y-extent ,number-pair? "See @code{extra-X-extent}.")
     (X-extent ,number-pair? "Hard coded extent in X direction. ")
     (Y-extent ,number-pair? "See @code{X-extent}.")
     (extra-offset ,number-pair? "A pair representing an offset. This
offset is added just before outputting the symbol, so the typesetting
engine is completely oblivious to it.")
     (finger-code ,symbol? "Code for the type of fingering indication in a
fret diagram.  Options include @code{none}, @code{in-dot}, and @code{below-string}.")
     (flag-count ,number? "The number of tremolo beams.")
     (flag-style ,symbol?
		 "a string determining what style of flag-glyph is
typeset on a Stem. Valid options include @code{()} and
@code{mensural}.  Additionally, @code{\"no-flag\"} switches off the
flag.")
     (font-family ,symbol? "The font family is the broadest category for selecting text fonts. Options include: @code{sans}, @code{roman} ")
     (font-encoding ,symbol? "The font encoding is the broadest
category for selecting a font. Options include: @code{fetaMusic},
@code{fetaNumber}, @code{TeX-text}, @code{TeX-math}, @code{fetaBraces},
@code{fetaDynamic}")
     (font-name ,string? "Specifies a file name (without extension) of
the font to load.  This setting override selection using
@code{font-family}, @code{font-series} and @code{font-shape}.")
     (font-size ,number? "The font size, compared the `normal'
size.  0 is style-sheet's normal size, -1 is smaller, +1 is bigger.
Each step of 1 is approximately 12% larger, 6 steps are exactly a
factor 2 larger. Fractional values are allowed.")
     (font-series ,symbol? "Select the series of a font. Choices
include @code{medium}, @code{bold}, @code{bold-narrow}, etc.")
     (font-shape ,symbol? "Select the shape of a font. Choices include @code{upright},
@code{italic}, @code{caps}.")
     (forced ,boolean? "manually forced accidental")
     (forced-distance ,ly:dimension? "A fixed distance between object
reference points in an alignment.")
     (force-hshift ,number? "This specifies a manual shift for notes
in collisions. The unit is the note head width of the first voice
note.  This is used by @internalsref{note-collision-interface}.")
     (fraction ,number-pair? "Numerator and denominator of a time
signature object.")
     (french-beaming ,boolean? "Use French beaming style for this
stem. The stem will stop at the innermost beams.")
     (fret-count ,integer? "The number of frets in a fret diagram.")
     ;; ugh: double, change.
     (full-size-change ,boolean? "Don't make a change clef smaller.")
     (non-default ,boolean? "Set for manually specified clefs.")
     (glyph ,string? "a string determining what (style) of glyph is
typeset. Valid choices depend on the function that is reading this
property.")
     (gap ,ly:dimension? "Size of a gap in a variable symbol.")
     (gap-count ,integer? "Number of gapped beams for tremolo.")
     (grow-direction ,ly:dir? "Crescendo or decrescendo?")
     (hair-thickness ,number? "Thickness of the thin line in a bar line.")
     (head-direction ,ly:dir? "Are the note heads left or right in a semitie?")
     (height ,ly:dimension? "Height of an object in staff-space.")
     (height-limit ,ly:dimension? "Maximum slur height: the longer the
slur, the closer it is to this height.")
     (horizontal-shift ,integer? "An integer that identifies ranking
of note-column for horizontal shifting. This is used by
@internalsref{note-collision-interface}.")
     (avoid-slur ,symbol? "Method of handling slur collisions.
Choices are @code{around}, @code{inside}, @code{outside}.  If unset, script
and slur ignore eachother.")
     (inspect-quants ,number-pair? "If debugging is set,
set beam/slur quant to this position, and print the respective scores.")
     (inspect-index ,integer? "If debugging is set,
set beam/slur configuration to this index, and print the respective scores.")
     (implicit ,boolean? "Is this an implicit bass figure?")
     (keep-inside-line ,boolean? "If set, this column cannot have
things sticking into the margin.")
     (kern ,ly:dimension? "Amount of extra white space to add. For
bar line, this is the amount of space after a thick line.")
     (knee ,boolean? "Is this beam kneed?")
     (knee-spacing-correction ,number? "Factor for the optical
correction amount for kneed beams. Set between 0 for no correction and
1 for full correction.")
     (label-dir ,ly:dir? "Side to which label is attached. @code{-1} for left, @code{1} for right.")
     (layer ,number? "The output layer [0..2]: layers define the order
of printing objects. Objects in lower layers are overprinted by
objects in higher layers.")
     (ledger-line-thickness ,number-pair?
			    "The thickness of ledger lines: it is the
sum of 2 numbers.  The first is the factor for line thickness, and the
second for staff space. Both contributions are added.")
     (left-padding ,ly:dimension? "The amount of space that is put
left to an object (eg. a group of accidentals).")
     (length ,ly:dimension? "User override for the stem length of
unbeamed stems.")
     (length-fraction ,number? "Multiplier for lengths. Used for
determining ledger lines and stem lengths.")
     (line-break-system-details ,list?
				"Alist of properties to use when this
column is the start of a system.")
     (line-count ,integer? "The number of staff lines.")
     (line-positions ,list? "Vertical positions of staff lines.")
     (line-thickness ,number? "The thickness of the tie/slur contour.")
     (long-text ,markup? "Text markup.  See @usermanref{Text markup}.")
     (max-beam-connect ,integer? "Maximum number of beams to connect
to beams from this stem. Further beams are typeset as beamlets.")
     (measure-length ,ly:moment? "Length of a
measure. Used in some spacing situations.")
     (measure-count ,integer? "The number of measures for a
multimeasure rest.")
     (merge-differently-headed ,boolean? "Merge
note heads in collisions, even if they have different note heads. The
smaller of the two heads will be rendered invisible. This used
polyphonic guitar notation. The value of this setting is used by
@internalsref{note-collision-interface} .")
     (merge-differently-dotted ,boolean? "Merge note heads in
collisions, even if they have a different number of dots. This normal
notation for some types of polyphonic music. ")
     (minimum-length-fraction ,number? "Minimum length of ledger line as fraction of note head size.")
     (minimum-distance ,ly:dimension? "Minimum distance between rest
and notes or beam.")
     (minimum-X-extent ,number-pair? "Minimum size of an object in X
dimension, measured in staff space.")
     (minimum-Y-extent ,number-pair? "See @code{minimum-X-extent}.")
     (minimum-length ,ly:dimension? "Try to make a spanner at least
this long. This requires an appropriate callback for the
@code{springs-and-rods} property.")
     (minimum-space ,ly:dimension? "Minimum distance that the victim
should move (after padding).")
     (neutral-direction ,ly:dir? "Which direction to take in the
center of the staff.")
     (neutral-position ,number? "Position (in half staff spaces) where
to flip the direction of custos stem.")
     (next ,ly:grob? "Object that is next relation (eg. the lyric syllable following an extender.")
     (note-names ,vector? "Vector of strings containing names for
easy-notation note heads.")
     (no-ledgers ,boolean? "If set, don't draw ledger lines on this object.")
     (no-spacing-rods ,boolean? "Items with this property do not cause
spacing constraints.")
     (no-stem-extend ,boolean? "If set, notes with ledger lines do not
get stems extending to the middle staff line.")
     (non-musical ,boolean? "True if the grob belongs in a NonMusicalPaperColumn.")
     (number-type ,symbol? "Type of numbers to use in label.  Choices
include @code{roman-lower}, @code{roman-upper}, and @code{arabic}.")
     (packed-spacing ,boolean? "If set, the notes are spaced as
tightly as possible.")
     (padding ,ly:dimension? "Add this much extra space between
objects that are next to each other.")
     (page-break-permission ,symbol? "Instructs the page breaker on whether to
put a page break at this column. Can be 'force, or 'allow.")
     (page-turn-permission ,symbol? "Instructs the page breaker on whether to
put a page turn at this column. Can be 'force, or 'allow.")
     (line-break-permission ,symbol? "Instructs the line breaker on whether to
put a line break at this column. Can be 'force, or 'allow.")
     (page-break-penalty ,number? "Penalty for page break at
this column. This affects the choices of the page breaker; it will avoid a page
break at a column with a positive penalty and prefer a page break at a column
with a negative penalty.")
     (page-turn-penalty ,number? "Penalty for a page turn at this column.
This affects the choices of the page breaker; it will avoid a page
turn at a column with a positive penalty and prefer a page turn at a column
with a negative penalty.")
     (line-break-penalty ,number? "Penalty for a line break at this column.
This affects the choices of the line breaker; it will avoid a line
break at a column with a positive penalty and prefer a line break at a column
with a negative penalty.")
     (positions ,pair?
		"Pair of staff coordinates @code{(@var{left}
. @var{right})}, where both @var{left} and @var{right} are in the
staff-space unit of the current staff.")
     (ratio ,number? "Parameter for slur shape. The higher this number, the
quicker the slur attains it @code{height-limit}.")
     (remove-empty ,boolean? "If set, remove group if it contains no
@code{interesting-items}")
     (remove-first ,boolean? "Remove the first staff of a orchestral score?")
     (rhythmic-location ,rhythmic-location? "Where (bar number, measure position) in the score.")
     (right-padding ,ly:dimension? "Space to insert on the right side  of an object (eg. between note and its accidentals.)")
     (rotation ,list? "Number of degrees to rotate this object, and what point
to rotate around. #'(45 0 0) means rotate 45 degrees around the center of this object.")
     (same-direction-correction ,number? "Optical correction amount
for stems that are placed in tight configurations. This amount is used
for stems with the same direction to compensate for note-head to stem distance.")
     (script-priority ,number? "A sorting key that determines in what
order a script is within a stack of scripts.")
     (self-alignment-X ,number? "Specify alignment of an object.  The
value -1 means left aligned, 0 centered, and 1 right-aligned in X
direction. Values in between may also be specified.")
     (self-alignment-Y ,number? "like @code{self-alignment-X} but for
Y axis.")
     (shorten-pair ,number-pair? "The lengths to shorten a
text-spanner on both sides, for example a pedal bracket.  Positive values
shorten the text-spanner, while negative values lengthen it.")
     (clip-edges ,boolean? "Allow outward pointing beamlets at the edges of beams?")
     (common-shortest-duration ,ly:moment?
			       "The most common shortest note length.
This is used in spacing. Enlarging this will set the score tighter.")
     (shortest-duration-space ,ly:dimension? "Start with this much
space for the shortest duration. This is expressed in
@code{spacing-increment} as unit. See also
@internalsref{spacing-spanner-interface}.")
     (shortest-playing-duration ,ly:moment? "The duration of the shortest playing here.")
     (shortest-starter-duration ,ly:moment? "The duration of the shortest
note that starts here.")
     (side-relative-direction ,ly:dir?
			      "Multiply direction of
@code{direction-source} with this to get the direction of this
object.")
     (side-axis ,number? "If the value is #X (or equivalently 1), the object is placed horizontally next to the other object. If the value is #Y or 0, it is placed vertically.")
     (size ,number? "Size of object, relative to standard size.")
     (slope ,number? "The slope of this object.")
     (slur-padding ,number? "Extra distance between slur and script.")
     (space-alist ,list? "A table that specifies distances between
prefatory items, like clef and time-signature. The format is an alist
of spacing tuples: @code{(@var{break-align-symbol} @var{type}
. @var{distance})}, where @var{type} can be the symbols
@code{minimum-space} or @code{extra-space}.")
     (spacing-increment ,number? "Add this much space for a doubled
duration. Typically, the width of a note head. See also
@internalsref{spacing-spanner-interface}.")
     (springs-and-rods ,boolean? "Dummy variable for triggering spacing routines.")
     (stacking-dir ,ly:dir? "Stack objects in which direction?")
     (staff-padding ,ly:dimension?
		    "Maintain this much space between reference points
and the staff.  Its effect is to align objects of differing
sizes (like the dynamic @b{p} and @b{f}) on their baselines.")
     (staff-position ,number? "Vertical position, measured in half
staff spaces, counted from the middle line.")
     (staff-space ,ly:dimension? "Amount of space between staff lines,
expressed in global staff-space.")
     (stemlet-length ,number? "How long should a stem over a rest be?")
     (stem-attachment ,number-pair? "A  (@var{x} . @var{y}) pair where the stem attaches to the notehead.")
     (stem-end-position ,number? "Where does the stem end (the end is opposite to the support-head.")
     ;;[TODO: doco]
     (stem-spacing-correction ,number? "Optical correction amount for
stems that are placed in tight configurations. For opposite
directions, this amount is the correction for two normal sized stems
that overlap completely.")
     (stencil ,ly:stencil? "The symbol to print.")
     (strict-note-spacing ,boolean? "If set, unbroken columns
with non-musical material (clefs, barlines, etc.) are not spaced
separately, but put before musical columns.")
     (strict-grace-spacing ,boolean? "If set, grace notes 
are not spaced separately, but put before musical columns.")
     (string-count ,integer? "The number of strings in a fret diagram.")
     (string-fret-finger-combinations ,list? "List consisting of (string-number fret-number finger-number) entries.")
     (stroke-style ,string? "set to \"grace\" to turn stroke through flag on.")
     (style ,symbol? "This setting determines in what style a grob is
typeset. Valid choices depend on the @code{stencil} callback reading
this property.")
     (text ,markup? "Text markup.  See @usermanref{Text markup}.")
;;FIXME -- Should both be the same?
     (text-direction ,ly:dir? "This controls the ordering of the
words. The default RIGHT is for roman text. Arabic or hebrew should
use LEFT.")
     (thick-thickness ,number? "Bar line thickness, measured in
@code{line-thickness}.")
     (thickness ,number? "Line thickness, generally measured in
@code{line-thickness}.")
     (thin-kern ,number? "The space after a hair-line in a bar line.")
     (threshold ,number-pair? "(@var{min} . @var{max}), where
@var{min} and @var{max} are dimensions in staff space.")
     (tie-configuration ,list? "List of (@var{position} . @var{dir})
pairs, indicating the desired tie configuration. A non-pair entry in
the list will cause said tie to be formatted automatically. ")
     (transparent ,boolean? "This makes the grob invisible.")
     (uniform-stretching ,boolean? "If set, items stretch proportional
to their durations. This looks better in complex polyphonic patterns")
     (used ,boolean? "If set, this spacing column is kept in the spacing problem")
     (when ,ly:moment? "Global time step associated with this column
happen?")
     (word-space ,ly:dimension? "space to insert between words in texts.")
     (width ,ly:dimension? "The width of a grob measured in staff space.")
     (zigzag-length ,ly:dimension? "The length of the lines of a
zigzag, relative to @code{zigzag-width}. A value of 1 gives 60-degree
zigzags.")
     (zigzag-width ,ly:dimension? "The width of one
zigzag-squiggle. This number will be adjusted slightly so that the
glissando line can be constructed from a whole number of squiggles.")
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
     (pure-relevant-elements ,ly:grob-array? "The subset of elements that are relevant for finding the pure-Y-extent.")
     (cached-pure-extents ,vector? "Used by a VerticalAxisGroup to cache the Y-extents of different column ranges.")
     (common-refpoint-of-elements ,ly:grob? "Caches the common_refpoint_of_array of the elements grob-set")
     (axis-group-parent-X ,ly:grob? "Containing X axis group")
     (axis-group-parent-Y ,ly:grob? "Containing Y axis group")
     (accidental-grobs ,list? "Alist with (NOTENAME . GROBLIST) entries")
     (adjacent-hairpins ,ly:grob-array? "List of directly neighboring hairpins")
     (all-elements ,ly:grob-array? "list of all grobs in this line. Its
function is to protect objects from being garbage collected.")
     (arpeggio ,ly:grob? "pointer to arpeggio object.")
     (beam ,ly:grob? "pointer to the beam, if applicable.")
     (bracket ,ly:grob? "the bracket for a  number.")
     (direction-source ,ly:grob? "in case side-relative-direction is
set, which grob to get the direction from .")
     (dot ,ly:grob? "reference to Dots object.")
     (dots ,ly:grob-array? "multiple Dots objects.")
     (figures ,ly:grob-array? "Figured bass objects for continuation line.")
     (important-column-ranks ,vector? "Cache of columns that contain items-worth-living.")
     (glyph-name ,string? "a name of character within font.")
     (pedal-text ,ly:grob? "Pointer to the text of a mixed-style piano pedal.")
     (stem ,ly:grob? "pointer to Stem object.")
     (tremolo-flag ,ly:grob? "The tremolo object on a stem.")
     (tie ,ly:grob? "")
     (staff-symbol ,ly:grob? "the staff symbol grob that we're in.")
     (rest ,ly:grob? "the staff symbol grob that we're in.")
     (rests ,ly:grob-array? "A list of rest objects.")
     (rest-collision ,ly:grob? "rest collision that a rest is in.")
     (accidental-grob ,ly:grob? "Accidental for this note.")
     (bars ,ly:grob-array? "list of bar line pointers.")
     (bounded-by-me ,ly:grob-array? "list of spanners that have this
column as start/begin point. Only columns that have grobs or act as
bounds are spaced.")
     (circled-tip ,boolean? "Put a circle at start/end of hairpins (al/del niente)")
     (columns ,ly:grob-array? "list of grobs, typically containing
paper-columns or note-column objects.")
     (conditional-elements ,ly:grob-array? "Internal use only")
     (encompass-objects ,ly:grob-array? "Objects that a slur should avoid
in addition to notes and stems.")
     (elements ,ly:grob-array? "list of grobs, type depending on the Grob
where this is set in.")
     (grace-spacing ,ly:grob? "a run of grace notes.")
     (spacing ,ly:grob? "the spacing spanner governing this section.")
     (heads ,ly:grob-array? "List of note heads.")
     (items-worth-living ,ly:grob-array? "A list of interesting items. If
empty in a particular staff, then that staff is erased.")
     (details ,list? "alist of parameters for detailed grob behavior.

more information on the allowed parameters can be found by inspecting
lily/slur-scoring.cc, lily/beam-quanting.cc, and
lily/tie-formatting-problem.cc.  Setting @code{debug-tie-scoring},
@code{debug-beam-scoring} or @code{debug-slur-scoring} also provides
useful clues.

")
     (note-heads ,ly:grob-array? "List of note head grobs")
     (note-head ,ly:grob? "A single note head")
     (separation-item ,ly:grob? "A separation item.")
     (side-support-elements ,ly:grob-array? "the support, a list of grobs.")
     (spacing-wishes ,ly:grob-array? "List of note spacing or staff spacing objects.")
     (stems ,ly:grob-array? "list of stem objects, corresponding to the notes that the arpeggio has to be before.")
     (tuplets ,ly:grob-array? "list of smaller tuplet brackets")
     (tuplet-number ,ly:grob? "the number for a bracket.")

     (left-neighbors ,ly:grob-array? " List of
spacing-wish grobs that are close to the current column.

The closest spacing-wishes determine the actual distances between the
columns.
")
     (right-neighbors ,ly:grob-array? "see left-neighbors")
     (left-items ,ly:grob-array? "")
     (right-items ,ly:grob-array? "")
     (cause ,scheme? "Any kind of causation objects (i.e. music, or perhaps translator) that was the cause for this grob.  ")
     (font ,ly:font-metric? "Cached font metric object")

     (positioning-done ,boolean?
		       "Used to signal that a positioning element
did its job. This ensures that a positioning is only done once.")
     (pure-Y-extent ,number-pair? "The estimated height of a system")


     (script-stencil ,pair? "Pair (@code{type} . @code{arg}), which
acts as an index for looking up a Stencil object.")

     (meta ,list? "Contains meta information. It is an alist with the
entries @code{name} and @code{interfaces}.")


     ;; TODO: use interface for this!
     (quantized-positions ,number-pair? "Beam positions after quanting.")
     (begin-of-line-visible ,boolean? "Used for marking ChordNames that should only show changes.")

     (quantize-position ,boolean? "If set, a vertical alignment is aligned to be within staff spaces.")
     (quant-score ,string? "Beam quanting score -- can be stored for
debugging")
     
     (least-squares-dy ,number? 
		       "ideal beam slope, without damping.")
     (stem-info ,pair? "caching of stem parameters")
     (note-columns ,pair? "list of NoteColumn grobs.")

;;; add-join would be enough if in ly:mensural-ligature::brew-ligature-primitive
;;; the next note could be seen
     (join-right-amount ,number? "")

     (delta-position ,number? "vertical position difference")
     (head-width ,ly:dimension? "width of this ligature head")

     ;; [TODO: change this]
     (primitive ,integer? "Pointer to a ligature primitive, i.e. an item similar to a note head that is part of a ligature. ")
     (ideal-distances ,list? "(@var{obj} . (@var{dist} . @var{strength})) pairs.")
     (minimum-distances ,list? "list of rods, that have the format (@var{obj} . @var{dist}).")

     (interfaces ,list? "list of symbols indicating the interfaces supported by this object. Is initialized from the @code{meta} field.")
     (shorten ,ly:dimension? "The amount of space that a
stem. Internally used to distribute beam shortening over stems. ")
     (slur ,ly:grob? "A pointer to a slur object")
     (use-breve-rest ,boolean? "Use breve rests for measures longer
than a whole rest.")
     

     (spaceable-staves ,ly:grob-array? "Objects to be spaced during page layout.")

     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; ancient notation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;;;;;; TODO:
     ;; there are too many properties for ancient notation
     ;; probably neume-types (a list of symbols) would also work.

     ;; However, such this list would consist of a couple of dozens of
     ;; entries, since head prefixes may be combined in many ways.  If
     ;; the macros in gregorian-init.ly would directly set prefix-set,
     ;; all the head prefixes could be junked; however, such macros
     ;; would be quite numerous, I guess.  --jr

     (auctum ,boolean? "is this neume liquescentically augmented?")
     (ascendens ,boolean? "is this neume of an ascending type?")
     (add-cauda ,boolean? "does this flexa require an additional cauda on the left side?")
     (add-join ,boolean? "is this ligature head joined with the next one by a vertical line?")
     (cavum ,boolean? "is this neume outlined?")
     (descendens ,boolean? "is this neume of a descendent type?")
     (deminutum ,boolean? "is this neume deminished?")
     (flexa-height ,ly:dimension? "height of a flexa shape in a ligature grob in staff_space.")
     (flexa-width ,ly:dimension? "width of a flexa shape in a ligature grob in staff_space.")
     (join-heads ,boolean? "Whether to join the note heads of an ambitus grob with a vertical line.")
     (linea ,boolean? "attach vertical lines to this neume?")
     (add-stem ,boolean? "is this ligature head a virga and therefore needs an additional stem on the right side?")
     (context-info ,integer? "Within a ligature, the final glyph or shape of a head may be affected by the left and/or right neighbour head.  context-info holds for each head such information about the left and right neighbour, encoded as a bit mask.")
     (inclinatum ,boolean? "is this neume an inclinatum?")
     (oriscus ,boolean? "is this neume an oriscus?")
     (quilisma ,boolean? "is this neume a quilisma?")
     (pes-or-flexa ,boolean? "shall this neume be joined with the previous head?")
     (prefix-set ,number? "a bit mask that holds all Gregorian head prefixes, such as @code{\\virga} or @code{\\quilisma}")
     (stropha ,boolean? "Is this neume a stropha?")
     (virga ,boolean? "Is this neume a virga?")
     (x-offset ,ly:dimension? "Extra horizontal offset for ligature heads.")

     ;; end ancient notation

     )))

(define-public all-backend-properties
  (append
   all-internal-grob-properties
   all-user-grob-properties))
