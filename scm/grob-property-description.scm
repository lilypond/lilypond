;;;; grob-property-description.scm -- part of generated backend documentation
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c) 1998--2001  Han-Wen Nienhuys <hanwen@cs.uu.nl>
;;;;                 Jan Nieuwenhuizen <janneke@gnu.org>



(define all-backend-properties '())

(define (grob-property-description symbol type? description)
  (if (not (equal? (object-property symbol 'backend-doc) #f))
      (begin
	(ly-warn (string-append "Redefining " (symbol->string symbol) "\n"))
	(exit 2)
      ))
  
  (set-object-property! symbol 'backend-type? type?)
  (set-object-property! symbol 'backend-doc description)
  (set! all-backend-properties (cons symbol all-backend-properties))
  )


  
;; put this in an alist?

(grob-property-description 'X-extent-callback procedure? "procedure taking an grob and axis argument, returning a number-pair. The return value is the extent of the grob.")
(grob-property-description 'X-offset-callbacks list? "list of functions, each taking an grob and axis argument. The function determine the position relative to this grob's parent. The last one in the list is called first.")
(grob-property-description 'Y-extent-callback procedure? "see @code{X-extent-callback}.")
(grob-property-description 'Y-offset-callbacks list? "see @code{X-offset-callbacks}.")
(grob-property-description 'after-line-breaking-callback procedure? "Procedure taking a grob as argument.
This procedure is called (using dependency resolution) after line breaking. Return value is ignored.")
(grob-property-description 'align number? "the alignment of the text, 0 is horizontal, 1 is vertical.")
(grob-property-description 'align-dir dir? "Which side to align? -1: left side, 0: around center of width, 1: right side.")
(grob-property-description 'alignment-done boolean? "boolean to administrate whether we've done the alignment already (to ensure that the process is done only once).")
(grob-property-description 'all-elements list? "list of all grobs in this line. Needed for protecting grobs from GC.")
(grob-property-description 'arch-angle number? "turning angle of the hook of a system brace" )
(grob-property-description 'arch-height number? "height of the hook of a system brace.")
(grob-property-description 'arch-thick number? "thickness of the hook of system brace.")
(grob-property-description 'arch-width number? "width of the hook of a system brace.")
(grob-property-description 'arithmetic-basicspace number? "see @ref{spacing-spanner-interface}.")
(grob-property-description 'arithmetic-multiplier number? "see @ref{spacing-spanner-interface}.")
(grob-property-description 'attachment pair? "cons of symbols, '(LEFT-TYPE . RIGHT-TYPE), where both types may be alongside-stem, stem, head or loose-end.")

(grob-property-description 'stem-attachment-function procedure? "Where
does the stem attach to the notehead? Function takes a symbol argument
being the style. It returns a (X . Y) pair, specifying location in
terms of note head bounding box.")
(grob-property-description 'attachment-offset pair? "cons of offsets,
'(LEFT-offset . RIGHT-offset).  This offset is added to the
attachments to prevent ugly slurs.  [fixme: we need more documentation here].
.")
(grob-property-description 'auto-knee-gap number-or-boolean? "the minimal smallest gap between two adjacent beamed chords for which beam will create auto-knees.  Set to false for no auto knees." )
(grob-property-description 'axes list? "list of axis numbers.
In the case of alignment grobs, this should contain only one number.")
(grob-property-description 'bar-size number? "size of a bar line.")
(grob-property-description 'bars list? "list of barline pointers.")
(grob-property-description 'bar-size-procedure procedure? "Procedure that computes the size of a bar line.")
(grob-property-description 'baseline-skip number? "Baseline skip to use for multiple lines of text.")
(grob-property-description 'bass list? " musical-pitch, optional.")
(grob-property-description 'beam ly-grob? "pointer to the beam, if applicable.")
(grob-property-description 'beam-space-function procedure? "function returning space given multiplicity.")
(grob-property-description 'beam-thickness number? "thickness, measured in staffspace.")
(grob-property-description 'beam-width number? "width of the tremolo sign.")
(grob-property-description 'beamAuto boolean? "enable autobeaming?.")
(grob-property-description 'beamed-lengths list? "list of stem lengths given beam multiplicity .")
(grob-property-description 'beamed-minimum-lengths list? "list of minimum stem lengths given beam multiplicity.")
(grob-property-description 'beamed-stem-shorten number? "shorten beamed stems in forced direction.")
(grob-property-description 'beaming number-pair? "number of beams extending to left and right.")
(grob-property-description 'beams list? "list of beam ptrs.")
(grob-property-description 'beautiful number? "number that dictates when a slur should be de-uglyfied.  It correlates with the enclosed area between noteheads and slurs.  A value of 0.1 yields only undisturbed slurs, a value of 5 will tolerate quite high blown slurs.")
(grob-property-description 'before-grace-spacing-factor number? " stretch space this much if there are grace notes before the column.")
(grob-property-description 'before-line-breaking-callback procedure? "Procedure taking grob as argument.
This procedure is called (using dependency resolution) before line breaking, but after generating discretionary items. Return value is ignored.")
(grob-property-description 'before-musical-spacing-factor number? "space before musical columns (eg. taken by accidentals) get this much
stretched when they follow a musical column, in absence of grace
notes.  0.0 means no extra space (accidentals are ignored).")
(grob-property-description 'between-system-string string? "string
 to dump between two systems. Useful for forcing pagebreaks.")
(grob-property-description 'bounded-by-me list? "list of spanners that have this
column as start/begin point. Only columns that have grobs or act as bounds are spaced.")
(grob-property-description 'bracket-thick number? "width of a system start bracket. .")
(grob-property-description 'break-align-symbol symbol? "the index in the spacing table (symbol) of the to be aligned item.")
(grob-property-description 'break-glyph-function procedure? "function taking glyph and break-direction, returning the glyph at a line break.")
(grob-property-description 'breakable boolean? "boolean indicating if this is a breakable item (clef, barline, key sig, etc.).")
(grob-property-description 'c0-position integer? "integer indicating the position of central C.")

(grob-property-description 'center-element ly-grob? "grob which will
be at the center of the group after aligning (when using
Align_interface::center_on_element). .")
(grob-property-description 'bar-line-collapse-height number? "Minimum height of system start delimiter bar-line glyphs.  If equal or smaller, the bar-line is removed.")
(grob-property-description 'brace-collapse-height number? "Minimum height of system start delimiter brace glyphs.  If equal or smaller, the brace is removed.")
(grob-property-description 'bracket-collapse-height number? "Minimum height of system start delimiter bracket glyphs.  If equal or smaller, the bracket is removed.")
(grob-property-description 'column-space-strength number? "relative strength of space following breakable columns (eg. prefatory matter).")
(grob-property-description 'columns list? "list of grobs, typically containing paper-columns, list of note-columns.")
(grob-property-description 'contains-grace boolean? "Used to widen entries for grace notes.")
(grob-property-description 'control-points list? "List of 4 offsets (number-pairs) that form control points for the  tie/slur shape.")
(grob-property-description 'damping integer? "amount of beam slope damping should beam slope be damped? 0: no, 1: yes, 100000: horizontal beams .")
(grob-property-description 'dash-length number? "the length of a dash.")
(grob-property-description 'dash-period number? "the length of one dash + white space.")
(grob-property-description 'dashed number? "[FIXME: use dash-period/dash length; see text-spanner] number representing the length of the dashes.")
(grob-property-description 'de-uglify-parameters list? "list of 3 real constants. They define the valid areas for the middle control points. Used in de_uglyfy. They are a bit empirical.")
(grob-property-description 'default-neutral-direction dir? "Where to go if we're in the middle of the staff.")
(grob-property-description 'delta-y number? "amount of ascension.")
(grob-property-description 'dependencies list? "list of score-grob pointers that indicate who to compute first for certain global passes.")
(grob-property-description 'details list? "alist of parameters for detailed grob behavior.")
(grob-property-description 'dir-forced boolean? "set if direction has been forced; read by Beam.")
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
(grob-property-description 'dir-list list? "list of stem directions, needed for optical spacing correction.")
(grob-property-description 'direction dir? "up or down, left or right?.")
(grob-property-description 'direction-source ly-grob? "in case side-relative-direction is set, which grob  to get the direction from .")
(grob-property-description 'dot ly-grob? "reference to Dots object.")
(grob-property-description 'dot-count integer? "number of dots.")
(grob-property-description 'duration-log integer? "2-log of the notehead duration, i.e. 0=whole note, 1 = half note, etc.")
(grob-property-description 'dy number? "set by beam: vertical travel height")
(grob-property-description 'edge-height pair? "a cons that specifies the heights of the vertical egdes '(LEFT-height . RIGHT-height).")
(grob-property-description 'edge-text pair? "a cons that specifies the texts to be set at the edges '(LEFT-text . RIGHT-text).")
(grob-property-description 'elements list? "list of grobs, type depending on the Grob where this is set in.")
(grob-property-description 'expand-limit integer? "maximum number of measures expanded in church rests.")
(grob-property-description 'extra-extent-X number-pair? "enlarge in X dimension by this much, measured in staff space.")
(grob-property-description 'extra-extent-Y number-pair? "see @code{extra-extent-Y}.")
(grob-property-description 'extra-offset number-pair? "pair of reals
(a cons) forcing an extra offset before outputting.
@code{extra-offset} is added just before `printing' the grob, so the
typesetting engine is completely oblivious to it.
")
(grob-property-description 'extra-space number-pair? "pair of distances (cons LEFT RIGHT).")
(grob-property-description 'extremity-offset-alist list? "an alist (attachment stem-dir*dir slur-dir*dir) -> offset.  The offset adds to the centre of the notehead, or stem.")

(grob-property-description 'extremity-rules list? "an alist (procedure
slur dir) -> attachment to determine the attachment (see above).  If
procedure returns #t, attachment is used.  Otherwise, the next
procedure is tried.")
(grob-property-description 'flag-style string? "style for flag (hook of a stem).")
(grob-property-description 'flag-width-function procedure? "Procedure that computes the width of a half-beam (a non-connecting beam.).")
(grob-property-description 'font-family symbol? "partial font
definition: music roman braces dynamic math ...")
(grob-property-description 'font-name symbol? "partial font definition:
base name of font file FIXME: should override other partials.")
(grob-property-description 'font-design-size number? "partial font definition: exact font size in points FIXME: should override font-relative-size.")
(grob-property-description 'font-relative-size number? "partial font definition: the relative size compared the `normal' size.
 0 is style-sheet's normal size, -1 is smaller, +1 is bigger, -1 is smaller.")
(grob-property-description 'font-series symbol? "partial font definition: medium, bold.")
(grob-property-description 'font-shape symbol? "partial font definition: upright or italic.")

(grob-property-description 'font-style symbol? "a precooked set of font
definitions, eg. finger volta timesig mark script large Large
dynamic.")

(grob-property-description 'force-hshift number? "amount of
collision_note_width that overides automatic collision settings. This
is used by @ref{note-collision-interface}.")

(grob-property-description 'fraction number-pair? "fraction of a time signature.")
(grob-property-description 'full-size-change boolean? "if set, don't make a change clef smaller.")

(grob-property-description 'glyph symbol? "a string determining what (style) of  glyph is typeset. Valid choices depend on the function that is reading this property. .")
(grob-property-description 'glyph-name string? "a name of character within font.")

(grob-property-description 'gap number? "Size of a gap in a variable symbol.")

(grob-property-description 'grow-direction dir? "crescendo or decrescendo?.")
(grob-property-description 'hair-thickness number? "thickness, measured in stafflinethickness.")
(grob-property-description 'heads pair? "list of note heads,

FIXME: in Tie this is a pair of grob pointers, pointing to the two heads of the  tie.

.")
(grob-property-description 'height number? "in staffspace.")
(grob-property-description 'height-quants procedure? "function of type (beam staff-line-thickness) -> list of quants.  Default value: default-beam-dy-quants.
.")
(grob-property-description 'horizontal-shift integer? "integer that identifies ranking of note-column for horizontal shifting. This is used by @ref{note-collision-interface}.")
(grob-property-description 'horizontal-space number? "amount of space to add after a note (in staff-space).")
(grob-property-description 'ideal-distances list? "(OBJ . (DIST . STRENGTH)) pairs.")
(grob-property-description 'interfaces list? "list of symbols indicating the interfaces supported by this object. Is initialized from the @code{meta} field.")
(grob-property-description 'inversion list? " musical-pitch, optional.")
(grob-property-description 'invisible-staff boolean? "is staff invisible?")
(grob-property-description 'items-worth-living list? "list of interesting items. If empty in a particular system, clear that system.")
(grob-property-description 'kern number? "amount of extra white space to add.

For text,  this is `relative'(?) to the current alignment.

For barline, space after a thick line.")
(grob-property-description 'layer number? "The output layer [0..2].  The default is 1.")
(grob-property-description 'left-padding number? "space left of accs.")
(grob-property-description 'length number? "Stem length for unbeamed stems, only for user override.")
(grob-property-description 'lengths list? "Stem length given multiplicity of flag.")
(grob-property-description 'line-count integer? "Number of staff lines.")
(grob-property-description 'line-thickness number? "the thickness[stafflinethickness] of the line.")
(grob-property-description 'lookup symbol? "lookup method: 'value for plain text, 'name for character-name.")
(grob-property-description 'magnify number? "the magnification factor.  FIXME: doesn't work for feta fonts.")
(grob-property-description 'maximum-duration-for-spacing moment? "space as if a duration of this type is available in this measure.")
(grob-property-description 'maximum-length number? "don't make Grob longer than this")
(grob-property-description 'maximum-rest-count integer? "kill off rests so we don't more than this number left.")
(grob-property-description 'merge-differently-dotted boolean? " Merge noteheads in collisions, even if they have a different number of dots. This normal notation for some types of polyphonic music. The value of this setting is used by @ref{note-collision-interface} .")
(grob-property-description 'minimum-distance number? "minimum distance between notes and rests.")
(grob-property-description 'minimum-distances list? "list of rods (ie. (OBJ . DIST) pairs).")
(grob-property-description 'minimum-extent-X number-pair? "minimum size in X dimension, measured in staff space.")
(grob-property-description 'minimum-extent-Y number-pair? "see @code{minimum-extent-Y}.")
(grob-property-description 'minimum-length number? "try to make the
Grob at least this long.

Also works as a scaling parameter for the length of hyphen. .")

;; FIXME.
(grob-property-description 'minimum-space number? "minimum distance that the victim should move (after padding).

FIXME: also pair? (cons LEFT RIGHT)

")

(grob-property-description 'minimum-width number? "minimum-width of rest symbol, in staffspace.")
(grob-property-description 'molecule-callback procedure? "Function taking grob as argument, returning a Scheme encoded Molecule.")
(grob-property-description 'new-accidentals list? "list of (pitch, accidental) pairs.")
(grob-property-description 'no-spacing-rods boolean? "read from grobs: boolean that makes Separation_item ignore this item (MOVE ME TO ITEM).")
(grob-property-description 'no-stem-extend boolean? "should stem not be extended to middle staff line?.")
(grob-property-description 'non-default boolean? "not set because of existence of a bar?.")
(grob-property-description 'note-character string? "character to print in a note head.")
(grob-property-description 'note-width number? "unit for horizontal translation, measured in staff-space.")
(grob-property-description 'number-gap number? "size of the gap for the number in a tuplet.")
(grob-property-description 'old-accidentals list? "list of (pitch, accidental) pairs.")
(grob-property-description 'origin ly-input-location? "location in input file of the definition.")
(grob-property-description 'outer-stem-length-limit number? "catch
suspect beam slopes, set slope to zero if outer stem is lengthened
more than this (in staffspace).")

(grob-property-description 'padding number? "add this much extra space between objects that are next to each other.")
(grob-property-description 'parallel-beam boolean? "internal: true if there is a beam just as wide as the bracket .")
(grob-property-description 'pitches list? "list of musical-pitch.")
(grob-property-description 'raise number? "height for text to be raised (a negative value lowers the text.")
(grob-property-description 'right-padding number? "space right of accs.")
(grob-property-description 'right-trim-amount number? "shortening of the lyric extender on the right.")
(grob-property-description 'script-priority number? "A sorting key that determines in what order a script is within a stack of scripts.")
(grob-property-description 'self-alignment-X number? "real number: -1 =
left aligned, 0 = center, 1 right-aligned in X direction.

 Set to an grob pointer, if you want that grob to be the center.
In this case, the center grob should have this object as a
reference point.
.")
(grob-property-description 'self-alignment-Y number? "like self-alignment-X but for Y axis.")
(grob-property-description 'shortest-playing-duration moment? "duration of the shortest playing in that column.")
(grob-property-description 'shortest-starter-duration moment? "duration of the shortest notes that starts exactly in this column.")
(grob-property-description 'side-relative-direction dir? "if set: get the direction from a different object, and multiply by this.")
(grob-property-description 'side-support list? "the support, a list of grobs.")
(grob-property-description 'slope number? "some kind of slope")
(grob-property-description 'slope-limit number? "set slope to zero if slope is running away steeper than this.")
(grob-property-description 'space-alist list? "Alist of break align spacing tuples. See basic-property.scm")
(grob-property-description 'space-function procedure? "function of type multiplicity -> real (in staffspace).")
(grob-property-description 'spacing-procedure procedure? "procedure
taking grob as argument. This is called after
before-line-breaking-callback, but before the actual line breaking
itself.  Return value is ignored.")
(grob-property-description 'stacking-dir dir? "stack contents of grobs in which direction ?.")
(grob-property-description 'staff-space number? "Amount of line leading relative to global staffspace.")
(grob-property-description 'staff-position number? "vertical position in staff spaces, counted from the middle line.")
(grob-property-description 'staff-symbol boolean? "the staff symbol grob that we're in.")
(grob-property-description 'staffline-clearance number? "don't get closer than this to stafflines.")
(grob-property-description 'stem ly-grob? "pointer to Stem object.")
(grob-property-description 'stem-end-position number? "Where does the stem end (the end is opposite to the support-head.")
(grob-property-description 'stem-length number? "length of stem.")
(grob-property-description 'stem-shorten list? "shorten stems in forced directions given flag multiplicity.")
(grob-property-description 'stem-spacing-correction number? "optical correction amount.")
(grob-property-description 'stems list? "list of stem objects, corresponding to the notes that the arpeggio has to be before.")
(grob-property-description 'stretch-distance number-pair? "pair of distances.")
(grob-property-description 'style symbol? "a string determining what style of  glyph is typeset. Valid choices depend on the function that is reading this property. .")
(grob-property-description 'support-head ly-grob? "the note head at
one end of the stem.")
(grob-property-description 'text markup? "
Scheme markup text.  It is defined as follows:

@example
text: string | (head? text+)
head: markup | (markup+)
markup-item: property | abbrev
property: (@var{key} . @var{value})
abbrev: @code{columns lines roman music bold italic named super sub overstrike text}
        @code{finger volta timesig mmrest mark script large Large dynamic}
@end example


The following abbreviations are currently defined:
@table @samp
@item columns
 horizontal mode: set all text on one line (default)
@item lines
 vertical mode: set every text on new line
@item roman
 select roman font
@item music
 select feta font, and lookup by character name
@item bold
 select bold series
@item italic
 select italic shape
@item named
 lookup by character name
@item text
 plain text lookup (by character value)
@item super
 superscript
@item sub
 subscript
@item overstrike
 the next text or character overstrikes this one
@item finger
 select fingering number fontstyle
@item volta
 select volta number fontstyle
@item timesig
 select time signature number fontstyle
@item mmrest
 select multi measure rest number fontstyle
@item mark
 select mark number fontstyle
@item script
 select scriptsize roman fontstyle
@item large
 select large roman fontstyle
@item Large
 select Large roman fontstyle
@item dynamic
 select dynamics fontstyle
@end table
.")
(grob-property-description 'thick number? "thickness, in stafflinethickness.")
(grob-property-description 'thick-thickness number? "thickness, measured in stafflinethickness.")
(grob-property-description 'thickness number? "thickness, measured in stafflinethickness.")
(grob-property-description 'thin-kern number? "space after a hair-line.")
(grob-property-description 'forced-distance number? "forced distance for an alignment.")
(grob-property-description 'threshold number-pair? "(cons MIN MAX), where MIN and MAX are dimensions in staffspace.")
(grob-property-description 'transparent boolean? "This is almost the
same as setting molecule-callback to #f, but this retains the
dimensions of this grob, which means that you can erase grobs
individually. .")
(grob-property-description 'tuplet-bracket-visibility boolean-or-symbol? "
This controls the visibility of the tuplet bracket.
Setting it to false will prevent printing of the
bracket. Setting the property to #'if-no-beam will make it
print only if there is no beam associated with this tuplet bracket.")
(grob-property-description 'tuplet-number-visibility boolean-or-symbol? "
Like @code{tuplet-bracket-visibility}, but for the number.")
(grob-property-description 'type symbol? "one of: line, dashed-line or dotted-line.")
(grob-property-description 'vertical-position-quant-function procedure? "
function of type (beam multiplicity dy staff-line-thickness) -> real.  Default value: default-beam-y-quants, also available: beam-traditional-y-quants.
.")
(grob-property-description 'visibility-lambda procedure? "a function that takes the break direction and returns a  cons of booleans containing (TRANSPARENT . EMPTY).")
(grob-property-description 'weird number? "urg?")
(grob-property-description 'when moment? "when does this column happen?.")
(grob-property-description 'word-space number? "elongate left by this much (FIXME: cumbersome semantics).")
(grob-property-description 'x-gap number? "horizontal gap between notehead and tie.")
(grob-property-description 'y-free number? "minimal vertical gap between slur and noteheads or stems.")
(grob-property-description 'y number? "set by beam: position of left edge.")
