;;;; grob-property-description.scm -- part of generated backend documentation
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c)  1998--2003  Han-Wen Nienhuys <hanwen@cs.uu.nl>
;;;;                 Jan Nieuwenhuizen <janneke@gnu.org>



(define-public all-backend-properties '())

(define (grob-property-description symbol type? description)
  (if (not (equal? (object-property symbol 'backend-doc) #f))
      (begin
	(ly:warn (string-append "Redefining " (symbol->string symbol) "\n"))
	(exit 2)
      ))
  
  (set-object-property! symbol 'backend-type? type?)
  (set-object-property! symbol 'backend-doc description)
  (set! all-backend-properties (cons symbol all-backend-properties))
  )

;; put this in an alist?

(grob-property-description 'X-extent-callback procedure? "procedure taking an grob and axis argument, returning a number-pair. The return value is the extent of the grob.

The size of a grob are determined through callbacks, settable with
grob properties @code{X-extent-callback} and @code{Y-extent-callback}.
There can be only one extent-callback for each axis. No callback
(Scheme value @code{#f}) means: `empty in this direction'. If you fill
in a pair of numbers, that pair hard-codes the extent in that
coordinate.
")
(grob-property-description 'X-offset-callbacks list? "list of functions, each taking an grob and axis argument. The function determine the position relative to this grob's parent. The last one in the list is called first.

Offsets of grobs are relative to a parent reference point. Most
positions are not known when an object is created, so these are
calculated as needed. This is done by adding a callback for a specific
direction.

Offset callbacks can be stacked, i.e.

@example
        \\property .... \\override #'Y-offset-callbacks = #(list
                callback1 callback2 callback3)

@end example

The callbacks will be executed in the order @code{callback3 callback2
callback1}. This is used for quantized positioning: the staccato dot is
above or below a note head, and it must not be on a staff-line.  To
achieve this, the staccato dot has two callbacks: one that positions the
grob above or below the note head, and one that rounds the Y-position of
the grob to the nearest open space.


")
(grob-property-description 'Y-extent-callback procedure? "see @code{X-extent-callback}.")
(grob-property-description 'Y-offset-callbacks list? "see @code{X-offset-callbacks}.")

(grob-property-description 'accidental-grobs list? "Alis with (NOTENAME . GROBLIST) entries")
(grob-property-description 'accidentals list? "List of alteration numbers.")
(grob-property-description 'add-cauda boolean? "does this flexa require an additional cauda on the left side?.")
(grob-property-description 'add-join boolean? "is this ligature head joined with the next one by a vertical line?")
(grob-property-description 'add-stem boolean? "is this ligature head a virga and therefore needs an additional stem on the right side?")
(grob-property-description 'adjust-if-on-staffline boolean? "If this grob is on a staff line, adjust its appearance, so that it better fits into the staff.  E.g., if set true on stem grobs, flares of mensural flags will always be aligned with the staff lines, regardless if the associated note head is printed on a staff line or inbetween.")
(grob-property-description 'after-line-breaking-callback procedure? "Procedure taking a grob as argument.
This procedure is called (using dependency resolution) after line breaking. Return value is ignored.")
(grob-property-description 'align-dir ly:dir? "Which side to align? -1: left side, 0: around center of width, 1: right side.")
(grob-property-description 'alignment-done boolean? "boolean to administrate whether we've done the alignment already (to ensure that the process is done only once).")
(grob-property-description 'all-elements grob-list? "list of all grobs in this line. Needed for protecting grobs from GC.")
(grob-property-description 'arch-angle number? "turning angle of the hook of a system brace" )
(grob-property-description 'arch-height ly:dimension? "height of the hook of a system brace.")
(grob-property-description 'arch-thick number? "thickness of the hook of system brace.")
(grob-property-description 'arch-width ly:dimension? "width of the hook of a system brace.")
(grob-property-description 'arpeggio ly:grob? "pointer to arpeggio object.") 
(grob-property-description 'arpeggio-direction ly:dir? "If set, put an
arrow on the arpeggio squiggly line.")
(grob-property-description 'ascendens boolean? "is this neume of an ascending?.")
(grob-property-description 'attachment pair? "cons of symbols
indicating how a slur should be attached at the ends. The format is
'(LEFT-TYPE . RIGHT-TYPE), where both TYPEs are symbols. The values of
these symbols may be alongside-stem, stem, head or loose-end.")
(grob-property-description 'attachment-offset pair? "cons of offsets,
'(LEFT-offset . RIGHT-offset).  This offset is added to the
attachments to prevent ugly slurs.  [fixme: we need more documentation here].
.")
(grob-property-description 'auctum boolean? "is this neume augmented?.")
(grob-property-description 'auto-knee-gap ly:dimension? "If a gap is found between noteheads
where a  horizontal beam fits that is larger than this number,  make a kneed beam.")
(grob-property-description 'axes list? "list of axis numbers.
In the case of alignment grobs, this should contain only one number.")
(grob-property-description 'bar-size ly:dimension? "size of a bar line.")
(grob-property-description 'bars grob-list? "list of barline pointers.")
(grob-property-description 'bar-size-procedure procedure? "Procedure that computes the size of a bar line.")
(grob-property-description 'base-shortest-duration ly:moment?
			   "Spacing is based on the shortest notes in a piece. Normally, pieces are spaced as if notes at least as short as this are present.")
(grob-property-description 'baseline-skip ly:dimension? "Baseline skip to use for multiple lines of text.")
(grob-property-description 'bass list? " musical-pitch, optional.")
(grob-property-description 'beam ly:grob? "pointer to the beam, if applicable.")
(grob-property-description 'beam-thickness ly:dimension? "thickness, measured in staffspace.")
(grob-property-description 'beam-width ly:dimension? "width of the tremolo sign.")
(grob-property-description 'beamed-lengths list? "list of stem lengths given beam multiplicity .")
(grob-property-description 'beamed-minimum-free-lengths list? "list of normal minimum free stem lengths (chord to beams) given beam multiplicity.")
(grob-property-description 'beamed-extreme-minimum-free-lengths list? "list of extreme minimum free stem lengths (chord to beams) given beam multiplicity.")

(grob-property-description 'beamed-stem-shorten list? "shorten beamed stems in forced direction.")
(grob-property-description 'beaming pair?
			   "Pair of number lists. Each number list
specifies which beams to make. 0 is the central beam, 1 is the next
beam toward the note etc. This information is used to determine how to
connect the beaming patterns from stem to stem inside a beam.")


(grob-property-description 'beautiful number? "number that dictates when a slur should be de-uglyfied.  It correlates with the enclosed area between noteheads and slurs.  A value of 0.1 yields only undisturbed slurs, a value of 5 will tolerate quite high blown slurs.")
(grob-property-description 'before-line-breaking-callback procedure? "Procedure taking grob as argument.
This procedure is called (using dependency resolution) before line breaking, but after generating discretionary items. Return value is ignored.")
(grob-property-description 'between-cols pair? "Where to attach a loose column to")
(grob-property-description 'between-system-string string? "string
 to dump between two systems. Useful for forcing pagebreaks.")
(grob-property-description 'bounded-by-me grob-list? "list of spanners that have this
column as start/begin point. Only columns that have grobs or act as bounds are spaced.")
(grob-property-description 'bracket-thick number? "width of a system start bracket. .")
(grob-property-description 'break-align-symbol symbol? "the index in the spacing table (symbol) of the to be aligned item.")
(grob-property-description 'break-glyph-function procedure? "function taking glyph and break-direction, returning the glyph at a line break.")
(grob-property-description 'breakable boolean? "boolean indicating if this is a breakable item (clef, barline, key sig, etc.).")
(grob-property-description 'c0-position integer? "integer indicating the position of central C.")
(grob-property-description 'cautionary-style symbol? "style  of cautionary accidentals. Choices are 'smaller (one size smaller) or 'parentheses.")
(grob-property-description 'cautionary boolean? "is this a cautionary accidentals.?")
(grob-property-description 'cavum boolean? "is this neume outlined?.")

(grob-property-description 'center-element ly:grob? "grob which will
be at the center of the group after aligning (when using
Align_interface::center_on_element). .")
(grob-property-description 'concaveness-gap ly:dimension? "A beam is
considered to be concave if the distance of an inner notehead to the
line between two outer noteheads is bigger than this gap.")
(grob-property-description 'concaveness-threshold number? "A beam is
considered to be concave is concaveness is bigger than this threshold.
Concaveness is calculated as the sum of the vertical distances of
inner noteheads that fall outside the interval of the two outer
noteheads, to the vertically nearest outer notehead, divided by the
square of the inner notes involved.")
(grob-property-description 'collapse-height ly:dimension? "Minimum height of system start delimiter.  If equal or smaller, the bracket is removed.")

(grob-property-description 'columns grob-list? "list of grobs, typically containing paper-columns.")
(grob-property-description 'conditional-elements grob-list? "Internal use only")
(grob-property-description 'context-info integer? "DOCME")

(grob-property-description 'control-points list? "List of 4 offsets (number-pairs) that form control points for the  tie/slur shape.")
(grob-property-description 'damping integer? "amount of beam slope damping should beam slope be damped? 0: no, 1: yes, 100000: horizontal beams .")
(grob-property-description 'dash-period number? "the length of one dash + white space.")
(grob-property-description 'dash-fraction number? "Size of the dashes, relative to dash-period. Should be between 0.0 (no line) and 1.0 (continuous line).")
(grob-property-description 'dashed number? "[FIXME: use dash-period/dash length; see text-spanner] number representing the length of the dashes.")
(grob-property-description 'descendens boolean? "is this neume of a descendent type?.")
(grob-property-description 'de-uglify-parameters list? "list of 3 real constants. They define the valid areas for the middle control points. Used in de_uglyfy. They are a bit empirical.")

(grob-property-description 'neutral-direction ly:dir? "Where to go if we're on the neutral position of the staff (by default, the middle of the staff; see also grob-property neutral-position).  [Ross] has the following to say about this: Some engravers consider the middle line neutral, and take the option of using either up- or down-stems for notes that fall on it. However, more up-to-date engraving no longer permits an option; now a down-stem is always appropriate.")
(grob-property-description 'neutral-position number? "Position (in half staff spaces) where to flip the direction of stems: by default, custodes above this position get their stems downwards; custodes below this position get their stems upwards.  A value of 0 designates the center of the staff.  Use property neutral-direction to control the behaviour of stems on the neutral position itself.  (Note: currently, neutral-position is supported only for custodes; for stems of note heads, neutral-position is currently fixed to 0, i.e. the middle of the staff.)")
(grob-property-description 'deminutum boolean? "is this neume deminished?.")
(grob-property-description 'dependencies grob-list? "list of score-grob pointers that indicate who to compute first for certain global passes.")
(grob-property-description 'details list? "alist of parameters for detailed grob behavior.")
(grob-property-description 'dir-function procedure? "function of type (count total)->direction.  Default value: beam-dir-majority, also available: beam-dir-mean, beam-dir-median.

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
(grob-property-description 'direction ly:dir? "up or down, left or right?.")
(grob-property-description 'direction-source ly:grob? "in case side-relative-direction is set, which grob  to get the direction from .")
(grob-property-description 'dot ly:grob? "reference to Dots object.")
(grob-property-description 'dot-count integer? "number of dots.")
(grob-property-description 'duration-log integer? "2-log of the notehead duration, i.e. 0=whole note, 1 = half note, etc.")
(grob-property-description 'edge-height pair? "a cons that specifies the heights of the vertical edges '(LEFT-height . RIGHT-height).")
(grob-property-description 'bracket-flare number-pair? "a pair
 that specifies how much edges of brackets  should slant outward.
 Value 0.0 means straight edges")

(grob-property-description 'edge-text pair? "a cons that specifies the texts to be set at the edges '(LEFT-text . RIGHT-text).")
(grob-property-description 'elements grob-list? "list of grobs, type depending on the Grob where this is set in.")
(grob-property-description 'expand-limit integer? "maximum number of measures expanded in church rests.")
(grob-property-description 'extra-X-extent number-pair? "enlarge in X dimension by this much, measured in staff space.")
(grob-property-description 'extra-Y-extent number-pair? "see @code{extra-Y-extent}.")
(grob-property-description 'X-extent number-pair? "Store extent. internal use only. ")
(grob-property-description 'Y-extent number-pair? "Store extent. internal use only. ")
(grob-property-description 'extra-offset number-pair? "pair of reals
(a cons) forcing an extra offset before outputting.
@code{extra-offset} is added just before `printing' the grob, so the
typesetting engine is completely oblivious to it.
")
(grob-property-description 'extremity-offset-alist list? "an alist (attachment stem-dir*dir slur-dir*dir) -> offset.  The offset adds to the centre of the notehead, or stem.")

(grob-property-description 'extremity-rules list? "an alist (procedure
slur dir) -> attachment to determine the attachment (see above).  If
procedure returns #t, attachment is used.  Otherwise, the next
procedure is tried.")
(grob-property-description
 'flag-style symbol?
 "a string determining what style of glyph is typeset on a Stem. Valid
options include undefined and mensural.
  Additionally, @code{no-flag} switches off the flag.")
(grob-property-description 'stroke-style string? "set to \"grace\" to turn stroke through flag on.")
(grob-property-description 'flag-width-function procedure? "Procedure that computes the width of a half-beam (a non-connecting beam.).")
(grob-property-description 'flexa-height ly:dimension? "height of a flexa shape in a ligature grob in staff_space.")
(grob-property-description 'flexa-width ly:dimension? "width of a flexa shape in a ligature grob in staff_space.")
(grob-property-description 'font-family symbol? "partial font
definition: music roman braces dynamic math ...")
(grob-property-description 'font-name string? "file name for the font to load.
Overrides all other font-X qualifiers.")
(grob-property-description 'font-design-size number? "partial font definition: exact font size in points FIXME: should override font-relative-size.")
(grob-property-description 'font-magnification number? "Magnification
  of the font. If undefined, the default is @code{1.0}.")

(grob-property-description 'font-relative-size number? "partial font
definition: the relative size compared the `normal' size.  0 is
style-sheet's normal size, -1 is smaller, +1 is bigger.")

(grob-property-description 'font-series symbol? "partial font definition: medium, bold.")
(grob-property-description 'font-shape symbol? "partial font definition: upright or italic.")

(grob-property-description 'force-hshift number? "amount of
collision_note_width that overides automatic collision settings. This
is used by @ref{note-collision-interface}.")

(grob-property-description 'fraction number-pair? "fraction of a time signature.")
(grob-property-description 'french-beaming boolean? "Use French
beaming style: stems stop at innermost beams.")
(grob-property-description 'full-size-change boolean? "if set, don't make a change clef smaller.")

(grob-property-description 'glyph string? "a string determining what (style) of  glyph is typeset. Valid choices depend on the function that is reading this property. .")
(grob-property-description 'glyph-name string? "a name of character within font.")
(grob-property-description 'glyph-name-procedure procedure? "Return
name of character within font.")

(grob-property-description 'gap ly:dimension? "Size of a gap in a variable symbol.")
(grob-property-description 'gap-count integer? "Number of gapped beams for tremolo.")

(grob-property-description 'grow-direction ly:dir? "crescendo or decrescendo?.")
(grob-property-description 'hair-thickness number? "thickness, measured in stafflinethickness.")
(grob-property-description 'heads pair? "Pair of grob pointers, pointing to the two heads of the  tie.")
(grob-property-description 'height ly:dimension? "in staffspace.")
(grob-property-description 'height-limit ly:dimension? "Maximum slur height,
  long slurs approach this height.

  For small width w, the height should be proportional to w, for w ->
  infinity, the height should rise to limit h_infinity asymptotically.

  Hence we take F (x) such that
@quotation
@example  
  F (0) = 0 , F' (0) = 1, and F (infinity) = 1
@end example
@end quotation
  where
@quotation
@example  
  h = height-limit * F (x * ratio / height-limit)
@end example
@end quotation
  Currently, for F we use
@quotation
@example  
  F (x) = 2/pi * atan (pi * x/2)
@end example
@end quotation
")
(grob-property-description 'horizontal-shift integer? "integer that identifies ranking of note-column for horizontal shifting. This is used by @ref{note-collision-interface}.")
(grob-property-description 'ideal-distances list? "(OBJ . (DIST . STRENGTH)) pairs.")
(grob-property-description 'inclinatum boolean? "is this neume an inclinatum?.")
(grob-property-description 'interfaces list? "list of symbols indicating the interfaces supported by this object. Is initialized from the @code{meta} field.")
(grob-property-description 'inversion list? " musical-pitch, optional.")
(grob-property-description 'items-worth-living grob-list? "list of interesting items. If empty in a particular system, clear that system.")
(grob-property-description 'join-heads boolean? "Whether to join the noteheads of an ambitus grob with a vertical line.")
(grob-property-description 'kern ly:dimension? "amount of extra white
space to add. For barline, space after a thick line.")
(grob-property-description 'knee boolean? "Is this beam a knee?")
(grob-property-description 'knee-spacing-correction number? "optical correction amount for knees. 0: no correction; 1: full correction.")
(grob-property-description 'layer number? "The output layer [0..2].  The default is 1.")

(grob-property-description 'left-position number? "position of left part of spanner.")
(grob-property-description 'left-padding ly:dimension? "space left of accs.")

(grob-property-description 'length ly:dimension? "Stem length for unbeamed stems, only for user override.")
(grob-property-description 'lengths list? "Stem length given
multiplicity of flag.  The Nth element of the list gives the stem
length of a note with N flags.
")
(grob-property-description 'linea boolean? "attach vertical lines to this neume?.")
(grob-property-description 'line-count integer? "Number of staff
lines.  If you want to override this for staffs individually, you must
use @code{\\outputproperty}. @code{\\property .. \\override} will not
work: @code{\\override} is processed after the StaffSymbol is created,
and will have no effect.
")
(grob-property-description 'maximum-length ly:dimension? "don't make Grob longer than this")
(grob-property-description 'maximum-rest-count integer? "kill off rests so we don't more than this number left.")
(grob-property-description 'measure-length ly:moment? "Length of a
measure. Used in some spacing situations.")
(grob-property-description 'measure-count integer? "number of measures for a multimeasure rest.")

(grob-property-description 'merge-differently-headed boolean? "Merge
noteheads in collisions, even if they have different note heads. The
smaller of the two heads will be rendered invisible. This used
polyphonic guitar notation. The value of this setting is used by
@ref{note-collision-interface} .")

(grob-property-description 'merge-differently-dotted boolean? " Merge
noteheads in collisions, even if they have a different number of
dots. This normal notation for some types of polyphonic music. The
value of this setting is used by @ref{note-collision-interface} .")

(grob-property-description 'meta list? "Alist of meta information of this grob.

The alist contains the following entries: name, interfaces.



")
(grob-property-description 'minimum-distance ly:dimension? "minimum distance between notes and rests.")
(grob-property-description 'minimum-distances list? "list of rods (ie. (OBJ . DIST) pairs).")

(grob-property-description 'minimum-X-extent number-pair? "minimum size in X dimension, measured in staff space.")
(grob-property-description 'minimum-Y-extent number-pair? "see @code{minimum-Y-extent}.")
(grob-property-description 'minimum-length ly:dimension? "try to make the
Grob at least this long.

Also works as a scaling parameter for the length of hyphen. .")

(grob-property-description 'minimum-space ly:dimension? "minimum distance that the victim should move (after padding).")

(grob-property-description 'molecule-callback procedure? "Function
taking grob as argument, returning a smobbed Molecule.

All visible, i.e. non-transparent, grobs have a callback to create a
Molecule. The callback should be a Scheme function taking one argument
(the grob) and returning a Molecule.  Most molecule callbacks are
written in C++, but you can also write them in Scheme. An example is
provided in @code{input/regression/molecule-hacking.ly}.
")

(grob-property-description 'molecule ly:molecule? "Cached output of the molecule-callback.")

(grob-property-description 'new-accidentals list? "list of (pitch, accidental) pairs.")
(grob-property-description 'no-spacing-rods boolean? "read from grobs: boolean that makes Separation_item ignore this item (MOVE ME TO ITEM).")
(grob-property-description 'no-stem-extend boolean? "should stem not be extended to middle staff line?.")
(grob-property-description 'non-default boolean? "not set because of existence of a bar?.")
(grob-property-description 'note-head-style string? "name of the font character to be used as note heads in the ambitus grob.")
(grob-property-description 'note-heads grob-list? "List of note head grobs")
(grob-property-description 'old-accidentals list? "list of (pitch, accidental) pairs.")
(grob-property-description 'oriscus boolean? "is this neume an oriscus?.")
(grob-property-description 'enclose-bounds boolean? "whether a text spanner should extend to the outer edge of the spanned notes")
(grob-property-description 'padding ly:dimension? "add this much extra space between objects that are next to each other.")
(grob-property-description 'pedal-text ly:grob? "Pointer to the text of a mixed-style piano pedal.")
(grob-property-description 'penalty number? "Penalty for breaking at
this column. 10000 or more means forbid linebreak, -10000 or less
means force linebreak.  Other values influence linebreaking decisions
as a real penalty.")

(grob-property-description 'pes-or-flexa boolean? "shall this neume be joined with the previous head?.")
(grob-property-description 'pitch-max ly:pitch? "FIXME, JUNKME")
(grob-property-description 'pitch-min ly:pitch? "FIXME, JUNKME")


(grob-property-description 'pitches list? "list of musical-pitch.")
(grob-property-description 'quilisma boolean? "is this neume a quilisma?.")
(grob-property-description 'positions pair? "cons of staff positions (LEFT . RIGHT")
(grob-property-description 'prefix-set number? "DOCME")
(grob-property-description 'ratio number? "Slur parameter.  See height-limit.")
(grob-property-description 'remove-first boolean?
			   "Remove the first staff of a orchestral score?")
(grob-property-description 'right-padding ly:dimension? "space right of accs.")
(grob-property-description 'right-position number? "position of right part of spanner.")
(grob-property-description 'right-trim-amount ly:dimension? "shortening of the lyric extender on the right.")
(grob-property-description 'script-priority number? "A sorting key that determines in what order a script is within a stack of scripts.")
(grob-property-description 'self-alignment-X number-or-grob? "real number: -1 =
left aligned, 0 = center, 1 right-aligned in X direction.

 Set to an grob pointer, if you want that grob to be the center.
In this case, the center grob should have this object as a
reference point.

TODO: revise typing.")
(grob-property-description 'self-alignment-Y number? "like self-alignment-X but for Y axis.")
(grob-property-description 'shorten ly:dimension? "the amount of space that a stem should be shortened (DOCME!)")
(grob-property-description 'shorten-pair number-pair? "the length on each side to shorten a text-spanner, for example a pedal bracket")
(grob-property-description 'common-shortest-duration ly:moment?
			   "The most common shortest note length.
This is used in spacing. Making this larger will make the score tighter.")
(grob-property-description 'shortest-duration-space ly:dimension? "Start
with this much space for the shortest duration. This is explessed in @code{spacing-increment} as unit. See also
@ref{spacing-spanner-interface}.")
(grob-property-description 'shortest-playing-duration ly:moment? "duration of the shortest playing in that column.")
(grob-property-description 'shortest-starter-duration ly:moment? "duration of the shortest notes that starts exactly in this column.")
(grob-property-description 'side-relative-direction ly:dir? "if set: get the direction from a different object, and multiply by this.")
(grob-property-description 'side-support-elements grob-list? "the support, a list of grobs.")
(grob-property-description 'slope number? "some kind of slope")
(grob-property-description 'slope-limit number? "set slope to zero if slope is running away steeper than this.")

(grob-property-description 'space-alist list? "Alist of break align
spacing tuples: format = (SYMBOL . (TYPE . DISTANCE)), where TYPE can be
minimum-space or extra-space.")
(grob-property-description 'space-function procedure? "return interbeam space given Beam grob and multiplicity.")
(grob-property-description 'spacing-increment number? "Add this much space for a doubled duration. Typically, the width of a note head. See also @ref{spacing-spanner-interface}.")

(grob-property-description 'spacing-wishes grob-list? "List of note spacing or staff spacing objects.")
(grob-property-description 'spacing-procedure procedure? "procedure
taking grob as argument. This is called after
before-line-breaking-callback, but before the actual line breaking
itself.  Return value is ignored.")
(grob-property-description 'stacking-dir ly:dir? "stack contents of grobs in which direction ?.")
(grob-property-description 'staff-space ly:dimension? "Amount of line leading relative to global staffspace.")
(grob-property-description 'staff-position number? "vertical position in staff spaces, counted from the middle line.")

(grob-property-description 'staffline-clearance ly:dimension? "don't get closer than this to stafflines.")
(grob-property-description 'stem ly:grob? "pointer to Stem object.")
(grob-property-description 'stem-attachment-function procedure? "Where
does the stem attach to the notehead? Function takes a symbol argument
being the style. It returns a (X . Y) pair, specifying location in
terms of note head bounding box.")
(grob-property-description 'stem-end-position number? "Where does the stem end (the end is opposite to the support-head.")

(grob-property-description 'stem-shorten list? "shorten stems in forced directions given flag multiplicity:
the Nth element of the list gives the amount stem shortening of a note with N flags.
")
(grob-property-description 'stem-spacing-correction number? "optical correction amount.  [TODO: doco] ")
(grob-property-description 'stems grob-list? "list of stem objects, corresponding to the notes that the arpeggio has to be before.")
(grob-property-description 'stropha boolean? "is this neume a stropha?.")
(grob-property-description 'style symbol? "a string determining what style of  glyph is typeset. Valid choices depend on the function that is reading this property. .")
(grob-property-description 'support-head ly:grob? "the note head at
one end of the stem.")
(grob-property-description 'text markup? "Text markup.  See the
notation manual for more information.")
(grob-property-description 'thick-thickness number? "thickness, measured in stafflinethickness.")
(grob-property-description 'thickness number? "thickness, measured in stafflinethickness.")
(grob-property-description 'thin-kern number? "space after a hair-line.")
(grob-property-description 'forced-distance ly:dimension? "forced distance for an alignment.")

(grob-property-description 'threshold number-pair? "(cons MIN MAX), where MIN and MAX are dimensions in staffspace.")
(grob-property-description 'transparent boolean? "This is almost the
same as setting molecule-callback to #f, but this retains the
dimensions of this grob, which means that you can erase grobs
individually. .")
(grob-property-description 'tremolo-flag ly:grob? "The tremolo object on a stem.")
(grob-property-description 'bracket-visibility boolean-or-symbol? "
This controls the visibility of the tuplet bracket.
Setting it to false will prevent printing of the
bracket. Setting the property to #'if-no-beam will make it
print only if there is no beam associated with this tuplet bracket.")
(grob-property-description 'number-visibility boolean-or-symbol? "
Like @code{bracket-visibility}, but for the number.")
(grob-property-description 'tie ly:grob? "") 
(grob-property-description 'break-visibility procedure? "a function
that takes the break direction and returns a cons of booleans
containing (TRANSPARENT . EMPTY).
")

(grob-property-description 'virga boolean? "is this neume a virga?.")
(grob-property-description 'when ly:moment? "when does this column happen?.")
(grob-property-description 'word-space ly:dimension? "elongate left (?) by this much (FIXME: cumbersome semantics).")

(grob-property-description 'alignment number? "alignment of lyrics on notehead, -1 is LEFT, 0 is CENTRE, 1 is RIGHT .")
(grob-property-description 'ignore-length-mismatch boolean? "if #t, stanzas with shorter lyrics can be moved away from their respective note-head by the lyric alignment code.")
(grob-property-description 'begin-alignment number? "proportion of lyric length from beginning to align with note-head for left-aligned lyrics.")
(grob-property-description 'end-alignment number? "proportion of lyric length from end to align with note-head for right-aligned lyrics.")
(grob-property-description 'width ly:dimension? "width of a grob measured in staff space.")
(grob-property-description 'width-correct ly:dimension? "width correction for (de)cresc. text spanners.")
(grob-property-description 'x-gap ly:dimension? "horizontal gap between notehead and tie.")
(grob-property-description 'x-offset ly:dimension? "extra horizontal offset for ligature heads.")
(grob-property-description 'y-free ly:dimension? "minimal vertical gap between slur and noteheads or stems.")
(grob-property-description 'y-offset ly:dimension? "extra vertical offset
for ties away from the center line.")
(grob-property-description 'zigzag-length ly:dimension? "The length of the
lines of a zigzag - relative to zigzag-width. a value of 1
gives 60-degree zigzags.")
(grob-property-description 'zigzag-width ly:dimension? "the width of one
zigzag-squiggle, measured in staff space. The width will be adjusted
so that the line can be constructed from a whole number of squiggles.")


;;; INTERNAL

(grob-property-description 'left-neighbors grob-list? " List of
spacing-wish grobs that are close to the current column.

The closest spacing-wishes determine the actual distances between the
columns.
")
(grob-property-description 'right-neighbors grob-list? "see left-neighbors")
(grob-property-description 'left-items grob-list? "")
(grob-property-description 'right-items grob-list? "")
(grob-property-description 'cause scheme? "Any kind of causation objects (i.e. music, or perhaps translator) that was the cause for this grob.  ")
(grob-property-description 'font ly:font-metric? "Cached font metric object")
(grob-property-description 'break-alignment-done boolean? "mark flag to signal we've done alignment already.")
(grob-property-description
 'staff-padding ly:dimension?
 "Maintain this much space to the staff.  It's effect is similar to
the padding mechanism, but this will keep objects above and below the
staff in a row more often, when the heights of the notes vary.
")

(grob-property-description 'staff-symbol ly:grob? "the staff symbol grob that we're in.")
(grob-property-description 'collision-done boolean? "")
(grob-property-description 'rest ly:grob? "the staff symbol grob that we're in.")
(grob-property-description 'rest-collision ly:grob? "rest collision that a rest is in.")
(grob-property-description 'rest-collision-done boolean? "Is this rest collision processed yet?")

(grob-property-description 'script-molecule pair? "Index code for script -- internal, see script.cc.")

(grob-property-description 'accidental-grob ly:grob? "Accidental for this note.")

(grob-property-description 'flag-count number? "")
(grob-property-description 'chord-tremolo boolean? "if set, this beam is a tremolo. TODO: use interface for this!")
(grob-property-description 'chord pair? "?")
(grob-property-description 'begin-of-line-visible boolean? "?")

(grob-property-description 'quant-score number? "Beam quanting score
-- can be stored for debugging")
(grob-property-description 'least-squares-dy number? 
 "ideal beam slope, without damping.")
(grob-property-description 'ligature-primitive-callback procedure? "callback that brews ligature head.")
(grob-property-description 'stem-info pair? "caching of stem parameters")
(grob-property-description 'note-columns pair? "list of NoteColumn grobs.")

(grob-property-description 'if-text-padding number? "padding in case texts are there.")
(grob-property-description 'grace-space-factor number? "space grace at this fraction of the increment.")
(grob-property-description 'position-callbacks list? "list of
functions set spanner positions.")

;;; Junk me, replace it by add-join.
(grob-property-description 'join-left boolean? "is this ligature head joined with the previous one by a vertical line?")

(grob-property-description 'join-left-amount number? "DOCME")

(grob-property-description 'delta-pitch number? "the interval between this and the next note, or, more precisely, their vertical distance; this is used in ligatures for calculation of the height of vertical joins flexa shapes")
(grob-property-description 'head-width ly:dimension? "width of this ligature head")
(grob-property-description 'primitive integer? "Pointer to a ligature primitive, i.e. an item similar to a note head that is part of a ligature. [TODO: change this]")
(grob-property-description 'minimum-beam-collision-distance ly:dimension?
"Minimum distance to beam for a rest collision.")

(grob-property-description 'avoid-note-head boolean? "if set, the stem of a chord does not pass through all note head, but start at the last note head. Used by tablature.")

(grob-property-description 'up-to-staff boolean? "if set, stems' lengths are set so as stems end out of the staff. Used by tablature.")

(grob-property-description 'use-breve-rest boolean? "boolean that
tells multi-measure-rest to use a breve rest to represent the duration
of 1 measure instead of whole rest.  It defaults to false.  It is set
to true when the duration of a measure is a breve or longer.")

; (display (length all-backend-properties))
