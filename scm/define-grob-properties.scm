;;;; grob-property-description.scm -- part of generated backend documentation
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c)  1998--2004  Han-Wen Nienhuys <hanwen@cs.uu.nl>
;;;;                 Jan Nieuwenhuizen <janneke@gnu.org>



(define (define-grob-property symbol type? description)
  (if (not (equal? (object-property symbol 'backend-doc) #f))
      (begin
	(ly:warn (string-append "Redefining " (symbol->string symbol) "\n"))
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
     (X-extent-callback ,procedure? "Procedure taking a grob and axis
argument, returning a number-pair. The return value is the extent of
the grob. If this value is set to @code{#f}, the object is empty in
the X direction.")

     (X-offset-callbacks ,list? "list of functions, each taking an grob and
axis argument. The function determine the position relative to this
grob's parent. The last one in the list is called first.")

     (Y-extent-callback ,procedure? "see @code{X-extent-callback}.")
     (Y-offset-callbacks ,list? "see @code{X-offset-callbacks}.")

     (accidentals ,list? "List of alteration numbers.")
     (add-cauda ,boolean? "does this flexa require an additional cauda on the left ,side?.")
     (add-join ,boolean? "is this ligature head joined with the next one by a vertical ,line?")
     (add-stem ,boolean? "is this ligature head a virga and therefore needs an additional stem on the right ,side?")
     (adjust-if-on-staffline ,boolean? "If this grob is on a staff line, adjust its appearance, so that it better fits into the staff.  E.g., if set true on stem grobs, flares of mensural flags will always be aligned with the staff lines, regardless if the associated note head is printed on a staff line or inbetween.")
     (after-line-breaking-callback ,procedure? "Procedure taking a grob as argument.
This procedure is called (using dependency resolution) after line breaking. Return value is ignored.")
     (align-dir ,ly:dir? "Which side to ,align? -1: left side, 0: around center of width, 1: right side.")
     (arch-angle ,number? "turning angle of the hook of a system brace" )
     (arch-height ,ly:dimension? "height of the hook of a system brace.")
     (arch-thick ,number? "thickness of the hook of system brace.")
     (arch-width ,ly:dimension? "width of the hook of a system brace.")
     (arpeggio-direction ,ly:dir? "If set, put an
arrow on the arpeggio squiggly line.")
     (ascendens ,boolean? "is this neume of an ,ascending?.")
     (attachment ,pair? "cons of symbols
indicating how a slur should be attached at the ends. The format is
'(LEFT-TYPE . RIGHT-TYPE), where both TYPEs are symbols. The values of
these symbols may be alongside-stem, stem, head or loose-end.")
     (attachment-offset ,pair? "cons of offsets,
'(LEFT-offset . RIGHT-offset).  This offset is added to the
attachments to prevent ugly slurs.  [fixme: we need more documentation here].
.")
     (auctum ,boolean? "is this neume ,augmented?.")
     (auto-knee-gap ,ly:dimension? "If a gap is found between noteheads
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
original molecule drawer to draw the balloon around.")


     (bar-size ,ly:dimension? "size of a bar line.")
     (bar-size-procedure ,procedure? "Procedure that computes the size of a bar line.")
     (base-shortest-duration ,ly:moment?
			     "Spacing is based on the shortest notes in a piece. Normally, pieces are spaced as if notes at least as short as this are present.")
     (baseline-skip ,ly:dimension? "Baseline skip to use for multiple lines of text.")
     (beam-thickness ,ly:dimension? "thickness, measured in staffspace.")
     (beam-width ,ly:dimension? "width of the tremolo sign.")
     (beamed-lengths ,list? "list of stem lengths given beam multiplicity .")
     (beamed-minimum-free-lengths ,list? "list of normal minimum free stem lengths (chord to beams) given beam multiplicity.")
     (beamed-extreme-minimum-free-lengths ,list? "list of extreme minimum free stem lengths (chord to beams) given beam multiplicity.")

     (beamed-stem-shorten ,list? "shorten beamed stems in forced direction.")
     (beaming ,pair?
	      "Pair of number lists. Each number list
specifies which beams to make. 0 is the central beam, 1 is the next
beam toward the note etc. This information is used to determine how to
connect the beaming patterns from stem to stem inside a beam.")


     (beautiful ,number? "number that dictates when a slur should be de-uglyfied.  It correlates with the enclosed area between noteheads and slurs.  A value of 0.1 yields only undisturbed slurs, a value of 5 will tolerate quite high blown slurs.")
     (before-line-breaking-callback ,procedure? "Procedure taking grob as argument.
This procedure is called (using dependency resolution) before line breaking, but after generating discretionary items. Return value is ignored.")
     (between-cols ,pair? "Where to attach a loose column to")
     (between-system-string ,string? "string
 to dump between two systems. Useful for forcing pagebreaks.")
     (bracket-thick ,number? "width of a system start bracket. .")
     (break-align-symbol ,symbol? "the index in the spacing table (symbol) of the to be aligned item.")
     (break-glyph-function ,procedure? "function taking glyph and break-direction, returning the glyph at a line break.")
     (breakable ,boolean? "boolean indicating if this is a breakable item (clef, barline, key sig, etc.).")
     (c0-position ,integer? "integer indicating the position of central C.")
     (cautionary-style ,symbol? "style  of cautionary accidentals. Choices are 'smaller (one size smaller) or 'parentheses.")
     (cautionary ,boolean? "is this a cautionary accidentals.?")
     (cavum ,boolean? "is this neume ,outlined?.")

     (concaveness-gap ,ly:dimension? "A beam is
considered to be concave if the distance of an inner notehead to the
line between two outer noteheads is bigger than this gap.")
     (concaveness-threshold ,number? "A beam is
considered to be concave is concaveness is bigger than this threshold.
Concaveness is calculated as the sum of the vertical distances of
inner noteheads that fall outside the interval of the two outer
noteheads, to the vertically nearest outer notehead, divided by the
square of the inner notes involved.")
     (collapse-height ,ly:dimension? "Minimum height of system start delimiter.  If equal or smaller, the bracket is removed.")

     ;;DOCME
     (context-info ,integer? "")

     (control-points ,list? "List of 4 offsets (number-pairs) that form control points for the  tie/slur shape.")

     (damping ,integer? "Amount of beam slope damping. 0: no, 1: yes, 100000: horizontal beams .")
     (dash-period ,number? "the length of one dash + white space. If
negative, no line is drawn at all.")
     
     (dash-fraction ,number? "Size of the dashes, relative to
dash-period. Should be between 0.0 (no line) and 1.0 (continuous
line).")

     ;; [FIXME: use dash-period/dash length; see text-spanner]
     (dashed ,number? " number representing the length of the dashes.")
     (descendens ,boolean? "is this neume of a descendent ,type?.")
     
     (de-uglify-parameters ,list? "list of 3 real constants. They
define the valid areas for the middle control points. Used in
de_uglyfy. They are empirical.")

     (neutral-direction ,ly:dir? "Where to go if we're on the neutral
position of the staff (see also grob-property neutral-position).")

     ;; todo: why is this tunable?
     (neutral-position ,number? "Position (in half staff spaces) where
to flip the direction of stems: by default, custodes above this
position get their stems downwards; custodes below this position get
their stems upwards.  A value of 0 designates the center of the staff.
Use property neutral-direction to control the behaviour of stems on
the neutral position itself.  (Note: currently, neutral-position is
supported only for custodes; for stems of note heads, neutral-position
is currently fixed to 0, i.e. the middle of the staff.)")
     
     (deminutum ,boolean? "is this neume ,deminished?.")
     (details ,list? "alist of parameters for detailed grob behavior.")
     (dir-function ,procedure? "function of type (count total)->direction.  Default value: beam-dir-majority, also available: beam-dir-mean, beam-dir-median.

The ways to calculate the direction of a beam work as follows:
@table @code
@item majority
number count of up or down notes
@item mean
mean center distance of all notes
@item median
mean centre distance weighted per note
@end table

")
     (direction ,ly:dir? "Up or down, left or right?.")
     (dot-count ,integer? "number of dots.")
     (duration-log ,integer? "2-log of the notehead duration, i.e. 0=whole note, 1 = half note, etc.")
     (edge-height ,pair? "a cons that specifies the heights of the vertical edges '(LEFT-height . RIGHT-height).")
     (bracket-flare ,number-pair? "a pair that specifies how much
edges of brackets should slant outward.  Value 0.0 means straight
edges")

     (edge-text ,pair? "a cons that specifies the texts to be set at the edges '(LEFT-text . RIGHT-text).")
     (expand-limit ,integer? "maximum number of measures expanded in church rests.")

     ;; remove me? 
     (extra-X-extent ,number-pair? "enlarge in X dimension by this much, measured in staff space.")
     (extra-Y-extent ,number-pair? "see @code{extra-Y-extent}.")

     
     (X-extent ,number-pair? "Store extent. internal use only. ")
     (Y-extent ,number-pair? "Store extent. internal use only. ")

     (extra-offset ,number-pair? "A pair representing an offset. This
offset is added just before `printing' the grob, so the typesetting
engine is completely oblivious to it.")

     (extremity-offset-alist ,list? "an alist (attachment stem-dir*dir
slur-dir*dir) -> offset.  The offset adds to the centre of the
notehead, or stem.")

     (extremity-rules ,list? "an alist (procedure
slur dir) -> attachment to determine the attachment (see above).  If
procedure returns #t, attachment is used.  Otherwise, the next
procedure is tried.")
     
     (flag-style ,symbol?
		 "a string determining what style of glyph is typeset on a Stem. Valid
options include undefined and mensural.  Additionally, @code{no-flag}
switches off the flag.")
     (stroke-style ,string? "set to \"grace\" to turn stroke through flag on.")
     (flag-width-function ,procedure? "Procedure that computes the width of a half-beam (a non-connecting beam.).")
     (flexa-height ,ly:dimension? "height of a flexa shape in a ligature grob in staff_space.")
     (flexa-width ,ly:dimension? "width of a flexa shape in a ligature grob in staff_space.")
     (font-family ,symbol? "partial font
definition: music roman braces dynamic math ...")
     (font-name ,string? "file name for the font to load.
Overrides all other font-X qualifiers.")
     (font-magnification ,number? "Magnification
  of the font. If undefined, the default is @code{1.0}.")

     (font-size ,number? "font definition: the relative size compared
the `normal' size.  0 is style-sheet's normal size, -1 is smaller, +1
is bigger.  Each step of 1 is approximately 12% larger, 6 steps are
exactly a factor 2 larger. Fractional values are allowed.")

     (font-series ,symbol? "partial font definition: medium, bold.")
     (font-shape ,symbol? "partial font definition: upright or italic.")

     (force-hshift ,number? "amount of collision_note_width that
overides automatic collision settings. This is used by
@internalsref{note-collision-interface}.")

     (fraction ,number-pair? "fraction of a time signature.")
     (french-beaming ,boolean? "Use French
beaming style: stems stop at innermost beams.")
     (full-size-change ,boolean? "if set, don't make a change clef smaller.")

     (glyph ,string? "a string determining what (style) of glyph is
typeset. Valid choices depend on the function that is reading this
property.")

     (glyph-name ,string? "a name of character within font.")
     (glyph-name-procedure ,procedure? "Return
name of character within font.")

     (gap ,ly:dimension? "Size of a gap in a variable symbol.")
     (gap-count ,integer? "Number of gapped beams for tremolo.")

     (grow-direction ,ly:dir? "crescendo or ,decrescendo?.")
     (hair-thickness ,number? "thickness, measured in linethickness.")
     (height ,ly:dimension? "in staffspace.")

     (height-limit ,ly:dimension? "Maximum slur height: the longer the
slur, the closer it is to this height.")

     (horizontal-shift ,integer? "integer that identifies ranking of
note-column for horizontal shifting. This is used by
@internalsref{note-collision-interface}.")
     (ideal-distances ,list? "(OBJ . (DIST . STRENGTH)) pairs.")
     (inclinatum ,boolean? "is this neume an ,inclinatum?.")
     (interfaces ,list? "list of symbols indicating the interfaces supported by this object. Is initialized from the @code{meta} field.")
     (join-heads ,boolean? "Whether to join the noteheads of an ambitus grob with a vertical line.")
     (kern ,ly:dimension? "amount of extra white
space to add. For barline, space after a thick line.")
     (knee ,boolean? "Is this beam a ,knee?")
     (knee-spacing-correction ,number? "optical correction amount for knees. 0: no correction; 1: full correction.")
     (layer ,number? "The output layer [0..2].  The default is 1.")

     (ledger-line-thickness ,number-pair?
			    "The thickness of ledger lines: it is the
sum of 2 numbers.  The car is the factor for linethickness, and the
cdr for staff space. Both contributions are added.")
     
     (left-position ,number? "position of left part of spanner.")
     (left-padding ,ly:dimension? "space left of accs.")

     (length ,ly:dimension? "Stem length for unbeamed stems, only for user override.")
     (lengths ,list? "Stem length given
multiplicity of flag.  The Nth element of the list gives the stem
length of a note with N flags.
")
     (linea ,boolean? "attach vertical lines to this ,neume?.")
     (line-count ,integer? "Number of staff
lines.  If you want to override this for staffs individually, you must
use @code{\\outputproperty}. @code{\\property .. \\override} will not
work: @code{\\override} is processed after the StaffSymbol is created,
and will have no effect.
")
     (maximum-rest-count ,integer? "kill off rests so we don't more than this number left.")
     (measure-length ,ly:moment? "Length of a
measure. Used in some spacing situations.")
     (measure-count ,integer? "number of measures for a multimeasure rest.")

     (merge-differently-headed ,boolean? "Merge
noteheads in collisions, even if they have different note heads. The
smaller of the two heads will be rendered invisible. This used
polyphonic guitar notation. The value of this setting is used by
@internalsref{note-collision-interface} .")

     (merge-differently-dotted ,boolean? " Merge
noteheads in collisions, even if they have a different number of
dots. This normal notation for some types of polyphonic music. The
value of this setting is used by @internalsref{note-collision-interface} .")

     (meta ,list? "Contains meta information. It is an alist with the
entries @code{name} and @code{interfaces}.")

     (minimum-distance ,ly:dimension? "Minimum distance between rest and notes or beam.")
     (minimum-distances ,list? "list of rods (ie. (OBJ . DIST) pairs).")
     (minimum-X-extent ,number-pair? "minimum size in X dimension, measured in staff space.")
     (minimum-Y-extent ,number-pair? "see @code{minimum-Y-extent}.")
     (minimum-length ,ly:dimension? "try to make the
Grob at least this long.

Also works as a scaling parameter for the length of hyphen. .")
     (minimum-space ,ly:dimension? "minimum distance that the victim should move (after padding).")
     (print-function ,procedure? "Function taking grob as argument,
returning a Molecule object.")

     (molecule ,ly:molecule? "Cached output of the print-function.")

     (new-accidentals ,list? "list of (pitch, accidental) pairs.")
     (no-spacing-rods ,boolean? "read from grobs: boolean that makes Separation_item ignore this item (MOVE ME TO ITEM).")
     (no-stem-extend ,boolean? "should stem not be extended to middle staff ,line?.")
     (non-default ,boolean? "not set because of existence of a ,bar?.")
     (note-head-style ,string? "name of the font character to be used as note heads in the ambitus grob.")
     (old-accidentals ,list? "list of (pitch, accidental) pairs.")
     (oriscus ,boolean? "is this neume an ,oriscus?.")

     (enclose-bounds ,number?
		     "How much of the bound a spanner  should enclose: +1 = completely, 0 = center, -1 not at all.")

     (padding ,ly:dimension? "add this much extra space between objects that are next to each other.")
     (penalty ,number? "Penalty for breaking at
this column. 10000 or more means forbid linebreak, -10000 or less
means force linebreak.  Other values influence linebreaking decisions
as a real penalty.")

     (pes-or-flexa ,boolean? "shall this neume be joined with the previous ,head?.")

     (pitch-max ,ly:pitch? "FIXME, JUNKME")
     (pitch-min ,ly:pitch? "FIXME, JUNKME")
     
     (quilisma ,boolean? "is this neume a ,quilisma?.")
     (positions ,pair?

		"cons of staff coordinates (@var{left} . @var{right}),
where both @var{left} and @var{right} are in the staff-space unit of
the current staff.")

     ;; DOCME
     (prefix-set ,number? "")
     (ratio ,number? "Parameter for slur shape. The higher this number, the
quicker the slur attains it @code{height-limit}.")
     (remove-first ,boolean?
		   "Remove the first staff of a orchestral score?")
     (right-padding ,ly:dimension? "space right of accs.")
     (right-position ,number? "position of right part of spanner.")
     (script-priority ,number? "A sorting key that determines in what order a script is within a stack of scripts.")

     ;; TODO: revise typing
     (self-alignment-X ,number-or-grob? "real number: -1 =
left aligned, 0 = center, 1 right-aligned in X direction.

 Set to an grob pointer, if you want that grob to be the center.
In this case, the center grob should have this object as a
reference point.

.")
     (self-alignment-Y ,number? "like self-alignment-X but for Y axis.")

     ;; DOCME
     (shorten ,ly:dimension? "the amount of space that a stem should be shortened ")
     (shorten-pair ,number-pair? "the length on each side to shorten a text-spanner, for example a pedal bracket")
     (common-shortest-duration ,ly:moment?
			       "The most common shortest note length.
This is used in spacing. Making this larger will make the score tighter.")
     (shortest-duration-space ,ly:dimension? "Start
with this much space for the shortest duration. This is explessed in @code{spacing-increment} as unit. See also
@internalsref{spacing-spanner-interface}.")
     (shortest-playing-duration ,ly:moment? "duration of the shortest playing in that column.")
     (shortest-starter-duration ,ly:moment? "duration of the shortest notes that starts exactly in this column.")
     (side-relative-direction ,ly:dir? "if set: get the direction from a different object, and multiply by this.")
     (slope ,number? "some kind of slope")
     (slope-limit ,number? "set slope to zero if slope is running away steeper than this.")

     (space-alist ,list? "Alist of break align
spacing tuples: format = (SYMBOL . (TYPE . DISTANCE)), where TYPE can be
minimum-space or extra-space.")
     (space-function ,procedure? "return interbeam space given Beam grob and multiplicity.")

     (spacing-increment ,number? "Add this much space for a doubled
duration. Typically, the width of a note head. See also
@internalsref{spacing-spanner-interface}.")

     (spacing-procedure ,procedure? "procedure taking grob as
argument. This is called after before-line-breaking-callback, but
before the actual line breaking itself.  Return value is ignored.")
     
     (stacking-dir ,ly:dir? "stack contents of grobs in which direction ?.")
     (staff-space ,ly:dimension? "Amount of line leading relative to global staffspace.")
     (staff-position ,number? "vertical position in staff spaces, counted from the middle line.")

     (staffline-clearance ,ly:dimension? "don't get closer than this to stafflines.")

     (stem-attachment-function ,procedure? "Where
does the stem attach to the ,notehead? Function takes grob and axis as
arguments. It returns a (X . Y) pair, specifying location in terms of
note head bounding box.")

     (stem-end-position ,number? "Where does the stem end (the end is opposite to the support-head.")

     (stem-shorten ,list? "shorten stems in forced directions given flag multiplicity:
the Nth element of the list gives the amount stem shortening of a note with N flags.
")
     ;;[TODO: doco]
     (stem-spacing-correction ,number? "optical correction amount.   ")
     (stropha ,boolean? "is this neume a ,stropha?.")
     (style ,symbol? "a string determining what style of  glyph is typeset. Valid choices depend on the function that is reading this property. .")
     (text-repeat-if-broken ,boolean?
			    "Repeat text on broken ,text-spanner?")
     (text ,markup? "Text markup.  See the
notation manual for more information.")
     (thick-thickness ,number? "thickness, measured in linethickness.")
     (thickness ,number? "thickness, measured in linethickness.")
     (thin-kern ,number? "space after a hair-line.")
     (forced-distance ,ly:dimension? "forced distance for an alignment.")

     (threshold ,number-pair? "(cons MIN MAX), where MIN and MAX are dimensions in staffspace.")
     (transparent ,boolean? "This is almost the
same as setting print-function to #f, but this retains the
dimensions of this grob, which means that you can erase grobs
individually. .")
     (bracket-visibility ,boolean-or-symbol? " This controls the
visibility of the tuplet bracket.  Setting it to false will prevent
printing of the bracket. Setting the property to #'if-no-beam will
make it print only if there is no beam associated with this tuplet
bracket.")
     
     (number-visibility ,boolean-or-symbol? " Like
@code{bracket-visibility}, but for the number.")

     ;; FIXME.
     (break-visibility ,procedure? "a function that takes the break
direction and returns a cons of booleans containing (TRANSPARENT
. EMPTY).  The following variables are predefined: @code{all-visible},
@code{begin-of-line-visible}, @code{end-of-line-visible},
@code{begin-of-line-invisible}, @code{end-of-line-invisible},
@code{all-invisible}.
")

     (virga ,boolean? "is this neume a ,virga?.")
     (when ,ly:moment? "when does this column ,happen?.")
     (word-space ,ly:dimension? "space to insert between lyrics or
words in texts.")
     (width ,ly:dimension? "width of a grob measured in staff space.")
     (x-gap ,ly:dimension? "horizontal gap between notehead and tie.")
     (x-offset ,ly:dimension? "extra horizontal offset for ligature heads.")
     (y-free ,ly:dimension? "minimal vertical gap between slur and noteheads or stems.")
     (y-offset ,ly:dimension? "extra vertical offset
for ties away from the center line.")
     (zigzag-length ,ly:dimension? "The length of the
lines of a zigzag - relative to zigzag-width. a value of 1
gives 60-degree zigzags.")
     (zigzag-width ,ly:dimension? "the width of one
zigzag-squiggle, measured in staff space. The width will be adjusted
so that the line can be constructed from a whole number of squiggles.")


     (avoid-note-head ,boolean? "if set, the stem of a chord does not pass through all note head, but start at the last note head. Used by tablature.")
     (staff-padding ,ly:dimension?
		    "Maintain this much space between reference points
and the staff.  Its effect is to align objects of differing sizes (like the dynamic @b{p} and @b{f})  on their baselines.")
     (use-breve-rest ,boolean? "boolean that tells multi-measure-rest
to use a breve rest to represent the duration of 1 measure instead of
whole rest.  It defaults to false.  It is set to true when the
duration of a measure is a breve or longer.")

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
     (accidental-grobs ,list? "Alis with (NOTENAME . GROBLIST) entries")
     
     (all-elements ,grob-list? "list of all grobs in this line. Needed for protecting grobs from GC.")
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
     (bars ,grob-list? "list of barline pointers.")
     (bounded-by-me ,grob-list? "list of spanners that have this
column as start/begin point. Only columns that have grobs or act as bounds are spaced.")
     (columns ,grob-list? "list of grobs, typically containing paper-columns.")
     (conditional-elements ,grob-list? "Internal use only")
     (dependencies ,grob-list? "list of score-grob pointers that indicate who to compute first for certain global passes.")
     (elements ,grob-list? "list of grobs, type depending on the Grob where this is set in.")
     (heads ,grob-list? "List of note heads.")
     (items-worth-living ,grob-list? "list of interesting items. If empty in a particular system, clear that system.")
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
did it's job. This ensures that a positioning is only done once.")


     (script-molecule ,pair? "Index code for script -- internal, see script.cc.")


     (flag-count ,number? "")

     ;; TODO: use interface for this!
     (chord-tremolo ,boolean? "if set, this beam is a tremolo. ")
     (begin-of-line-visible ,boolean? "Used for marking ChordNames that should only show changes.")
     (head-pair ,pair? "Pair of grob pointers, pointing to the two heads of the tie.")
     (quant-score ,number? "Beam quanting score -- can be stored for
debugging")
     (least-squares-dy ,number? 
		       "ideal beam slope, without damping.")
     (ligature-primitive-callback ,procedure? "callback that brews ligature head.")
     (stem-info ,pair? "caching of stem parameters")
     (note-columns ,pair? "list of NoteColumn grobs.")

     (if-text-padding ,number? "padding in case texts are there.")
     (grace-space-factor ,number? "space grace at this fraction of the increment.")
     (position-callbacks ,list? "list of
functions set spanner positions.")

;;; Junk me, replace it by add-join.
     (join-left ,boolean? "is this ligature head joined with the previous one by a vertical ,line?")

     (join-left-amount ,number? "")

     (delta-pitch ,number? "the interval between this and the next note, or, more precisely, their vertical distance; this is used in ligatures for calculation of the height of vertical joins flexa shapes")
     (head-width ,ly:dimension? "width of this ligature head")

     ;; [TODO: change this]
     (primitive ,integer? "Pointer to a ligature primitive, i.e. an item similar to a note head that is part of a ligature. ")
     
     )))

(define-public all-backend-properties
  (append
   all-internal-grob-properties
   all-user-grob-properties))

