;;;; grob-property-description.scm -- part of generated backend documentation
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c)  1998--2004  Han-Wen Nienhuys <hanwen@cs.uu.nl>
;;;;                 Jan Nieuwenhuizen <janneke@gnu.org>



(define (define-grob-property symbol type? description)
  (if (not (equal? (object-property symbol 'backend-doc) #f))
      (begin
	(ly:warn-append "Redefining ~S" symbol)
	(exit 2)
      ))
  
  (set-object-property! symbol 'backend-type? type?)
  (set-object-property! symbol 'backend-doc description)
  symbol
  )

;; put this in an alist?
(define-public
  all-user-grob-properties

  (map
   (lambda (x)
     (apply define-grob-property x))

   `(
     (X-extent-callback ,procedure? "Procedure that calculates the
extent of this object. If this value is set to @code{#f}, the object
is empty in the X direction.  The procedure takes a grob and axis
argument, and returns a number-pair.
")

     (X-offset-callbacks ,list? "A list of functions determining this
objects' position relative to its parent. The last one in the list is
called first.  The functions take a grob and axis argument. ")

     (Y-extent-callback ,procedure? "see @code{X-extent-callback}.")
     (Y-offset-callbacks ,list? "see @code{X-offset-callbacks}.")

     (accidentals ,list? "List of alteration numbers.")
     (add-stem-support ,boolean? "If set, the Stem object is included in this script's support") 
    
     (align-dir ,ly:dir? "Which side to align? @code{-1}: left side,
@code{0}: around center of width, @code{1}: right side.")
     (arch-angle ,number? "Turning angle of the hook of a system brace" )
     (arch-height ,ly:dimension? "Height of the hook of a system brace.")
     (arch-thick ,number? "Thickness of the hook of system brace.")
     (arch-width ,ly:dimension? "Width of the hook of a system brace.")
     (arpeggio-direction ,ly:dir? "If set, put an
arrow on the arpeggio squiggly line.")
  
     (auto-knee-gap ,ly:dimension? "If a gap is found between note heads
where a  horizontal beam fits that is larger than this number,  make a kneed beam.")
     (axes ,list? "list of axis numbers.
In the case of alignment grobs, this should contain only one number.")

     (balloon-text ,markup? "Text to add to help balloon")
     (balloon-text-props ,list? "Font properties
for balloon text.")
     (balloon-text-offset ,number-pair?
			  "Where to put text relative to balloon.")
     (balloon-padding ,ly:dimension? "Text to add to help balloon")
     (balloon-original-callback ,procedure? "The
original stencil drawer to draw the balloon around.")


     (bar-size ,ly:dimension? "size of a bar line.")
     (bar-size-procedure ,procedure? "Procedure that computes the size of a bar line.")
     (barre-type ,symbol? "Type of barre indication used in a fret diagram.
Choices include @code{curved} and @code{straight}.")
     (base-shortest-duration ,ly:moment?
			     "Spacing is based on the shortest notes in a piece. Normally, pieces are spaced as if notes at least as short as this are present.")
     (baseline-skip ,ly:dimension? "Distance between base lines of
 multiple lines of text.")
     (beam-thickness ,ly:dimension? "thickness, measured in staffspace.")
     (beam-width ,ly:dimension? "width of the tremolo sign.")
     (beamed-lengths ,list? "list of stem lengths given beam multiplicity .")
     (beamed-minimum-free-lengths ,list? "list of normal minimum free stem lengths (chord to beams) given beam multiplicity.")
     (beamed-extreme-minimum-free-lengths ,list? "list of extreme minimum free stem lengths (chord to beams) given beam multiplicity.")

     (beamed-stem-shorten ,list? "How much to shorten beamed stems,
when their direction is forced. It is a  list, since the value is different
depending on the number flags/beams.")
     (beaming ,pair?
	      "Pair of number lists. Each number list specifies which
beams to make. 0 is the central beam, 1 is the next beam toward the
note etc. This information is used to determine how to connect the
beaming patterns from stem to stem inside a beam.")


     (before-line-breaking-callback ,procedure? "This procedure is
called before line breaking, but after splitting breakable items at
potential line breaks.")
     (between-cols ,pair? "Where to attach a loose column to")
     (bound-padding ,number? "The amount of padding to insert around spanner bounds.")
     (bracket-flare ,number-pair? "A pair of numbers specifying how
much edges of brackets should slant outward.  Value 0.0 means straight
edges")
     (bracket-thick ,number? "width of a system start bracket.")
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
     (break-glyph-function ,procedure? "This function determines the
appearance of a bar line at the line break.  It takes a glyph and
break-direction and returns the glyph at a line break.")
     (breakable ,boolean? "Can this object appear at a line break,
like clefs and bar lines?")
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

     ;;DOCME

     (control-points ,list? "List of 4 offsets (number-pairs) that form control points for the  tie/slur shape.")

     (damping ,number? "Amount of beam slope damping. 0: no, 1: yes,
100000: horizontal beams.")
     (dash-period ,number? "the length of one dash + white space. If
negative, no line is drawn at all.")
     
     (dash-fraction ,number? "Size of the dashes, relative to
dash-period. Should be between 0.0 (no line) and 1.0 (continuous
line).")

     ;; todo: why is this tunable?
     (dir-function ,procedure? "The function to determine the
direction of a beam. Choices include: 

@table @code
@item  beam-dir-majority
number count of up or down notes
@item beam-dir-mean
mean center distance of all notes
@item  beam-dir-median.
mean center distance weighted per note
@end table

")
     
     (direction ,ly:dir? "Up or down, left or right?")
     (dot-color ,symbol? "Color of dots.  Options include 
@code{black} and @code{white}.")
     (dot-radius ,number? "Radius of dots.")
     (dot-count ,integer? "The number of dots.")
     (duration-log ,integer? "The 2-log of the note head duration, i.e. 0=whole note, 1 = half note, etc.")
     (edge-height ,pair? "A pair of number specifying the heights of
the vertical edges '(@var{left-height} . @var{right-height}).")
     (edge-text ,pair? "A pair specifying the texts to be set at the
edges '(@var{left-text} . @var{right-text}).")
     (excentricity ,number? "How asymmetrical to make a slur. Positive means move the center to the right.")
     
     (expand-limit ,integer? "maximum number of measures expanded in church rests.")

     ;; remove me? 
     (extra-X-extent ,number-pair? "A grob is enlarged in X dimension
by this much.")
     (extra-Y-extent ,number-pair? "See @code{extra-Y-extent}.")
     
     (X-extent ,number-pair? "Hard coded extent in X direction. ")
     (Y-extent ,number-pair? "See @code{X-extent}.")

     (extra-offset ,number-pair? "A pair representing an offset. This
offset is added just before outputting the symbol, so the typesetting
engine is completely oblivious to it.")

     (finger-code ,symbol? "Code for the type of fingering indication in a
fret diagram.  Options include @code{none}, @code{in-dot}, and @code{below-string}.")
     (flag-style ,symbol?
		 "a string determining what style of flag-glyph is
typeset on a Stem. Valid options include @code{()} and
@code{mensural}.  Additionally, @code{\"no-flag\"} switches off the
flag.")
     (flag-width-function ,procedure? "Procedure that computes the width of a half-beam (a non-connecting beam.).")
     (font-family ,symbol? "The font family is the broadest category for selecting text fonts. Options include: @code{sans}, @code{roman} ")
     (font-encoding ,symbol? "The font encoding is the broadest
category for selecting a font. Options include: @code{fetaMusic},
@code{fetaNumber}, @code{TeX-text}, @code{TeX-math}, @code{fetaBraces},
@code{fetaDynamic}")
     (font-name ,string? "Specifies a file name (without extension) of
the font to load.  This setting override selection using
@code{font-family}, @code{font-series} and @code{font-shape}.")
     (font-magnification ,number? "Magnification of the font, when it
is selected with @code{font-name}.")

     (font-size ,number? "The font size, compared the `normal'
size.  0 is style-sheet's normal size, -1 is smaller, +1 is bigger.
Each step of 1 is approximately 12% larger, 6 steps are exactly a
factor 2 larger. Fractional values are allowed.")

     (font-series ,symbol? "Select the series of a font. Choices
include @code{medium}, @code{bold}, @code{bold-narrow}, etc.")
     (font-shape ,symbol? "Select the shape of a font. Choices include @code{upright},
@code{italic}, @code{caps}.")

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
     (glyph-name ,string? "a name of character within font.")
     (glyph-name-procedure ,procedure? "Return the name of a character
within font, to use for printing a symbol.")

     (gap ,ly:dimension? "Size of a gap in a variable symbol.")
     (gap-count ,integer? "Number of gapped beams for tremolo.")
     (grace-space-factor ,number? "Space grace notes at this fraction
of the @code{spacing-increment}.")

     (grow-direction ,ly:dir? "Crescendo or decrescendo?")
     (hair-thickness ,number? "Thickness of the thin line in a bar line.")
     (height ,ly:dimension? "Height of an object in staffspace.")
     (height-limit ,ly:dimension? "Maximum slur height: the longer the
slur, the closer it is to this height.")

     (horizontal-shift ,integer? "An integer that identifies ranking
of note-column for horizontal shifting. This is used by
@internalsref{note-collision-interface}.")
     (inside-slur ,boolean? "If set, this script should be inside the
slur. If set to false, the script places itself around the slur. If
unset, script and slur ignore each other ")
     (inspect-quants ,number-pair? "If debugging is set,
set beam quant to this position, and print the respective scores.")
     
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
     (left-position ,number? "Vertical position of left part of spanner.")
     (left-padding ,ly:dimension? "The amount of space that is put
left to a group of accidentals.")
     (length ,ly:dimension? "User override for the stem length of
unbeamed stems.")
     (lengths ,list? "Default stem lengths. The list gives a length
for each flag-count.")
     (line-count ,integer? "The number of staff lines.")
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

     (minimum-distance ,ly:dimension? "Minimum distance between rest
and notes or beam.")
     (minimum-X-extent ,number-pair? "Minimum size of an object in X
dimension, measured in staff space.")
     (minimum-Y-extent ,number-pair? "See @code{minimum-Y-extent}.")
     (minimum-length ,ly:dimension? "Try to make a spanner at least
this long. This requires an appropriate routine for the
@code{spacing-procedure} property.")
     (minimum-space ,ly:dimension? "Minimum distance that the victim
should move (after padding).")
     (print-function ,procedure? "Function taking grob as argument,
returning a @code{Stencil} object.")
     (neutral-direction ,ly:dir? "Which direction to take in the
center of the staff.")
     (neutral-position ,number? "Position (in half staff spaces) where
to flip the direction of custos stem.")

     (new-accidentals ,list? "List of @code{(@var{pitch}
. @var{accidental})} pairs.")
     (next ,ly:grob? "Object that is next relation (eg. the lyric syllable following an extender.")
     (note-names ,vector? "Vector of strings containing names for
easy-notation note heads.")
     (no-ledgers ,boolean? "If set, don't draw ledger lines on this object.")
     (no-spacing-rods ,boolean? "Items with this property do not cause
spacing constraints.")
     (no-stem-extend ,boolean? "If set, notes with ledger lines do not
get stems extending to the middle staff line.")
     (number-type ,symbol? "Type of numbers to use in label.  Choices
include @code{roman-lower}, @code{roman-upper}, and @code{arabic}.")
     
     (old-accidentals ,list? "List of @code{(@var{pitch} . @var{accidental})
pairs.}")

     (enclose-bounds ,number?
		     "How much of the bound a spanner should enclose:
+1 = completely, 0 = center, -1 not at all.")

     (padding ,ly:dimension? "Add this much extra space between
objects that are next to each other.")
     (page-penalty ,number? "Penalty for page break at
this column.  10000 or more means forbid linebreak, -10000 or less
means force page break.  Other values influence page breaking decisions
as a real penalty.")
     (penalty ,number? "Penalty for line break at
this column.  10000 or more means forbid line break, -10000 or less
means force line break.  Other values influence line breaking decisions
as a real penalty.")

     (positions ,pair?
		"Pair of staff coordinates @code{(@var{left}
. @var{right})}, where both @var{left} and @var{right} are in the
staff-space unit of the current staff.")

     (ratio ,number? "Parameter for slur shape. The higher this number, the
quicker the slur attains it @code{height-limit}.")
     (remove-first ,boolean? "Remove the first staff of a orchestral score?")
     (right-padding ,ly:dimension? "Space to insert between note and
accidentals.")
     (right-position ,number? "Vertical position of right part of spanner.")
     (script-priority ,number? "A sorting key that determines in what
order a script is within a stack of scripts.")

     (self-alignment-X ,number? "Specify alignment of an object.  The
value -1 means left aligned, 0 centered, and 1 right-aligned in X
direction. Values in between may also be specified.")
     (self-alignment-Y ,number? "like @code{self-alignment-X} but for
Y axis.")

     (shorten-pair ,number-pair? "The lengths to shorten a
text-spanner on both sides, for example a pedal bracket")
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
     (size ,number? "Size of object, relative to standard size.")
     (slope ,number? "The slope of this object.")
     (slur-padding ,number? "Extra distance between slur and script.")
     (slur-details ,list?
		   "An alist of scoring parameters for slur formatting")
     (space-alist ,list? "A table that specifies distances between
prefatory items, like clef and time-signature. The format is an alist
of spacing tuples: @code{(@var{break-align-symbol} @var{type}
. @var{distance})}, where @var{type} can be the symbols
@code{minimum-space} or @code{extra-space}.")
     (space-function ,procedure? "Calculate the vertical space between
two beams. This function takes a beam grob and the maximum number of
beams.")

     (spacing-increment ,number? "Add this much space for a doubled
duration. Typically, the width of a note head. See also
@internalsref{spacing-spanner-interface}.")

     (spacing-procedure ,procedure? "Procedure for calculating spacing
parameters.  The routine is called after
@code{before-line-breaking-callback}.")
     (stacking-dir ,ly:dir? "Stack objects in which direction?")
     (staff-space ,ly:dimension? "Amount of space between staff lines,
expressed in global staffspace.")
     (staff-position ,number? "Vertical position, measured in half
staff spaces, counted from the middle line.")
     (staffline-clearance ,ly:dimension? "How far away ties keep from
staff lines.")
     (stem-attachment-function ,procedure? "A function that calculates
where a stem attaches to the note head? This is a fallback when this
information is not specified in the font.  The function takes a grob
and axis argument, and returns a (@var{x} . @var{y}) pair, specifying
location in terms of note head bounding box.")

     (stem-end-position ,number? "Where does the stem end (the end is opposite to the support-head.")

     (stem-shorten ,list? "How much a stem in a forced direction
should be shortened. The list gives an amount depending on the number
of flags/beams.")

     ;;[TODO: doco]
     (stem-spacing-correction ,number? "Optical correction amount for
stems that are placed in tight configurations. For opposite
directions, this amount is the correction for two normal sized stems
that overlap completely.")

     
     (string-count ,integer? "The number of strings in a fret diagram.")
     (stroke-style ,string? "set to \"grace\" to turn stroke through flag on.")
     
     (style ,symbol? "This setting determines in what style a grob is
typeset. Valid choices depend on the @code{print-function} that is
reading this property.")
     (text ,markup? "Text markup.  See @usermanref{Text markup}.")
;;FIXME -- Should both be the same?
     (thick-thickness ,number? "Bar line thickness, measured in
@code{linethickness}.")
     (thickness ,number? "Bar line thickness, measured in
@code{linethickness}.")
     (thin-kern ,number? "The space after a hair-line in a bar line.")

     (threshold ,number-pair? "(@var{min} . @var{max}), where
@var{min} and @var{max} are dimensions in staff space.")
     (transparent ,boolean? "This is almost the same as setting
@code{print-function} to @code{#f}, but this retains the dimensions of
this grob, which means that grobs can be erased individually.")
     (bracket-visibility ,boolean-or-symbol? "This controls the
visibility of the tuplet bracket.  Setting it to false will prevent
printing of the bracket. Setting the property to @code{'if-no-beam}
will make it print only if there is no beam associated with this
tuplet bracket.")
     (number-visibility ,boolean-or-symbol? "Like
@code{bracket-visibility}, but for the number.")

     ;; FIXME.
     (break-visibility ,procedure? "A function that takes the break
direction and returns a cons of booleans containing (@var{transparent}
. @var{empty}).  The following variables are predefined:
@code{all-visible}, @code{begin-of-line-visible},
@code{end-of-line-visible}, @code{begin-of-line-invisible},
@code{end-of-line-invisible}, @code{all-invisible}.")
     (flag-count ,number? "The number of tremolo beams.")

     (when ,ly:moment? "Global time step associated with this column
happen?")
     (word-space ,ly:dimension? "space to insert between lyrics or
words in texts.")
     (width ,ly:dimension? "The width of a grob measured in staff space.")
     (x-gap ,ly:dimension? "The horizontal gap between note head and tie.")
     (y-offset ,ly:dimension? "Extra vertical offset for ties away
from the center line.")
     (zigzag-length ,ly:dimension? "The length of the lines of a
zigzag, relative to @code{zigzag-width}. A value of 1 gives 60-degree
zigzags.")
     (zigzag-width ,ly:dimension? "The width of one
zigzag-squiggle. This number will be adjusted slightly so that the
glissando line can be constructed from a whole number of squiggles.")

     (avoid-note-head ,boolean? "If set, the stem of a chord does not
pass through all note heads, but starts at the last note head. ")
     (staff-padding ,ly:dimension?
		    "Maintain this much space between reference points
and the staff.  Its effect is to align objects of differing
sizes (like the dynamic @b{p} and @b{f}) on their baselines.")

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
     
     (accidental-grobs ,list? "Alist with (NOTENAME . GROBLIST) entries")
     (adjacent-hairpins ,grob-list? "List of directly neighboring hairpins")
     (after-line-breaking-callback ,procedure? "This procedure is called after line breaking. Its return value is ignored.")     
     (all-elements ,grob-list? "list of all grobs in this line. Its
function is to protect objects from being garbage collected.")
     (arpeggio ,ly:grob? "pointer to arpeggio object.")
     (beam ,ly:grob? "pointer to the beam, if applicable.")
     (center-element ,ly:grob? "grob which will be at the center of
the group after aligning (when using
Align_interface::center_on_element).")
     (direction-source ,ly:grob? "in case side-relative-direction is
set, which grob to get the direction from .")
     (dot ,ly:grob? "reference to Dots object.")
     (pedal-text ,ly:grob? "Pointer to the text of a mixed-style piano pedal.")
     (stem ,ly:grob? "pointer to Stem object.")
     (tremolo-flag ,ly:grob? "The tremolo object on a stem.")
     (tie ,ly:grob? "")
     (staff-symbol ,ly:grob? "the staff symbol grob that we're in.")
     (rest ,ly:grob? "the staff symbol grob that we're in.")
     (rest-collision ,ly:grob? "rest collision that a rest is in.")
     (accidental-grob ,ly:grob? "Accidental for this note.")
     (bars ,grob-list? "list of bar line pointers.")
     (bounded-by-me ,grob-list? "list of spanners that have this
column as start/begin point. Only columns that have grobs or act as bounds are spaced.")
     (columns ,grob-list? "list of grobs, typically containing
paper-columns or note-column objects.")
     (conditional-elements ,grob-list? "Internal use only")
     (dependencies ,grob-list? "list of score-grob pointers that indicate who to compute first for certain global passes.")
     (encompass-objects ,grob-list? "Objects that a slur should avoid
in addition to notes and stems.")
     (elements ,grob-list? "list of grobs, type depending on the Grob where this is set in.")
     (heads ,grob-list? "List of note heads.")
     (items-worth-living ,grob-list? "A list of interesting items. If
empty in a particular staff, then that staff is erased.")
     (details ,list? "alist of parameters for detailed grob behavior.")
     (note-heads ,grob-list? "List of note head grobs")
     (side-support-elements ,grob-list? "the support, a list of grobs.")
     (spacing-wishes ,grob-list? "List of note spacing or staff spacing objects.")
     (stems ,grob-list? "list of stem objects, corresponding to the notes that the arpeggio has to be before.")


     (left-neighbors ,grob-list? " List of
spacing-wish grobs that are close to the current column.

The closest spacing-wishes determine the actual distances between the
columns.
")
     (right-neighbors ,grob-list? "see left-neighbors")
     (left-items ,grob-list? "")
     (right-items ,grob-list? "")
     (cause ,scheme? "Any kind of causation objects (i.e. music, or perhaps translator) that was the cause for this grob.  ")
     (font ,ly:font-metric? "Cached font metric object")

     (positioning-done ,boolean?
		       "Used to signal that a positioning element
did its job. This ensures that a positioning is only done once.")


     (script-stencil ,pair? "Pair (@code{type} . @code{arg}), which
acts as an index for looking up a Stencil object.")

     (meta ,list? "Contains meta information. It is an alist with the
entries @code{name} and @code{interfaces}.")


     ;; TODO: use interface for this!
     (chord-tremolo ,boolean? "if set, this beam is a tremolo. ")
     (begin-of-line-visible ,boolean? "Used for marking ChordNames that should only show changes.")
     (head-pair ,pair? "Pair of grob pointers, pointing to the two heads of the tie.")
     (quant-score ,string? "Beam quanting score -- can be stored for
debugging")
     (least-squares-dy ,number? 
		       "ideal beam slope, without damping.")
     (ligature-primitive-callback ,procedure? "callback that brews ligature head.")
     (stem-info ,pair? "caching of stem parameters")
     (note-columns ,pair? "list of NoteColumn grobs.")

     (position-callbacks ,list? "list of
functions set spanner positions.")

;;; Junk me, replace it by add-join.
     (join-left-amount ,number? "")

     (delta-pitch ,number? "the interval between this and the next note, or, more precisely, their vertical distance; this is used in ligatures for calculation of the height of vertical joins flexa shapes")
     (head-width ,ly:dimension? "width of this ligature head")

     ;; [TODO: change this]
     (primitive ,integer? "Pointer to a ligature primitive, i.e. an item similar to a note head that is part of a ligature. ")
     (stencil ,ly:stencil? "Cached output of the print-function.")
     (ideal-distances ,list? "(@var{obj} . (@var{dist} . @var{strength})) pairs.")
     (minimum-distances ,list? "list of rods, that have the format (@var{obj} . @var{dist}).")

     (interfaces ,list? "list of symbols indicating the interfaces supported by this object. Is initialized from the @code{meta} field.")
     (shorten ,ly:dimension? "The amount of space that a
stem. Internally used to distribute beam shortening over stems. ")
     (slur  ,ly:grob? "A pointer to a slur object")
     (use-breve-rest ,boolean? "Use breve rests for measures longer
than a whole rest.")
     

     (spaceable-staves ,grob-list? "Objects to be spaced during page layout.")

     ;; ancient notation

     ;;;;;;; TODO:
     ;; there are too many properties for ancient notation
     ;; probably neume-types (a list of symbols) would also work.
     
     (auctum ,boolean? "is this neume augmented?")
     (ascendens ,boolean? "is this neume of an ascending?")
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
     (join-left ,boolean? "is this ligature head joined with the previous one by a vertical line?")
     (context-info ,integer? "DOCME")
     (inclinatum ,boolean? "is this neume an inclinatum?")
     (oriscus ,boolean? "is this neume an oriscus?")
     (quilisma ,boolean? "is this neume a quilisma?")
     (pes-or-flexa ,boolean? "shall this neume be joined with the previous head?")
     ;; DOCME
     (prefix-set ,number? "")
     (stropha ,boolean? "Is this neume a stropha?")
     (virga ,boolean? "Is this neume a virga?")
     (x-offset ,ly:dimension? "Extra horizontal offset for ligature heads.")
     
     ;; end ancient notation

     )))

(define-public all-backend-properties
  (append
   all-internal-grob-properties
   all-user-grob-properties))
