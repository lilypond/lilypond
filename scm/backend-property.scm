(define all-backend-properties '())

(define (elt-property-description symbol type? description)
  (set-object-property! symbol 'backend-type? type?)
  (set-object-property! symbol 'backend-doc description)
  (set! all-backend-properties (cons symbol all-backend-properties))
  )


  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(elt-property-description 'X-extent-callback procedure? "procedure taking an element and axis argument, returning a number-pair. The return value is the extent of the element.")
(elt-property-description 'X-offset-callbacks list? "list of functions, each taking an element and axis argument. The function determine the position relative to this element's parent. The last one in the list is called first")
(elt-property-description 'Y-extent-callback procedure? "see @code{X-extent-callback}")
(elt-property-description 'Y-offset-callbacks list? "see @code{X-offset-callbacks}")
(elt-property-description 'after-line-breaking-callback procedure? "Procedure taking graphical element as argument.
This procedure is called (using dependency resolution) after line breaking. Return value is ignored")
(elt-property-description 'align number? "the alignment of the text, 0 is horizontal, 1 is vertical")
(elt-property-description 'align-dir dir? "Which side to align? -1: left side, 0: around center of width, 1: right side")
(elt-property-description 'alignment-done boolean? "boolean to administrate whether we've done the alignment already (to ensure that the process is done only once)")
(elt-property-description 'all-elements list? "list of all score elements in this line. Needed for protecting elements from GC.")
(elt-property-description 'arch-angle number? "")
(elt-property-description 'arch-height number? "")
(elt-property-description 'arch-thick number? "")
(elt-property-description 'arch-width number? "")
(elt-property-description 'arithmetic-basicspace number? "")
(elt-property-description 'arithmetic-multiplier number? "see arithmetic-basicspace")
(elt-property-description 'attachment pair? "cons of symbols, '(LEFT-TYPE . RIGHT-TYPE), where both types may be alongside-stem, stem, head or loose-end")
(elt-property-description 'attachment-offset pair? "cons of offsets, '(LEFT-offset . RIGHT-offset).  This offset is added to the attachments to prevent ugly slurs.")
(elt-property-description 'axes list? "list of axis numbers. Should contain only one number.")
(elt-property-description 'axes list? "list of axis (number) in which this group works")
(elt-property-description 'bar-size number? "")
(elt-property-description 'bars list? "list of barline ptrs.")
(elt-property-description 'barsize-procedure procedure? "how to compute the size of a bar line")
(elt-property-description 'bass list? " musical-pitch, optional")
(elt-property-description 'beam ly-grob? "pointer to the beam, if applicable")
(elt-property-description 'beam-space-function procedure? "function returning space given multiplicity")
(elt-property-description 'beam-space-function procedure? "function returning space given multiplicity")
(elt-property-description 'beam-thickness number? "thickness, measured in staffspace")
(elt-property-description 'beam-thickness number? "thickness, measured in staffspace")
(elt-property-description 'beam-width number? "width of the tremolo sign")
(elt-property-description 'beam-width number? "width of the tremolo sign")
(elt-property-description 'beamed-lengths list? "list of stem lengths given beam multiplicity ")
(elt-property-description 'beamed-minimum-lengths list? "list of minimum stem lengths given beam multiplicity")
(elt-property-description 'beamed-stem-shorten number? "shorten beamed stems in forced direction")
(elt-property-description 'beaming number-pair? "number of beams extending to left and right")
(elt-property-description 'beams list? "list of beam ptrs.")
(elt-property-description 'beautiful number? "number that dictates when a slur should be de-uglyfied.  It correlates with the enclosed area between noteheads and slurs.  A value of 0.1 yields only undisturbed slurs, a value of 5 will tolerate quite high blown slurs.")
(elt-property-description 'before-grace-spacing-factor number? " stretch space this much if there are grace notes before the column")
(elt-property-description 'before-line-breaking-callback procedure? "Procedure taking graphical element as argument.
This procedure is called (using dependency resolution) before line breaking, but after generating discretionary items. Return value is ignored")
(elt-property-description 'before-musical-spacing-factor number? "space before musical columns (eg. taken by accidentals) get this much
stretched when they follow a musical column, in absence of grace
notes.  0.0 means no extra space (accidentals are ignored)")
(elt-property-description 'between-system-string string? "string
 to dump between two systems. Useful for forcing pagebreaks")
(elt-property-description 'bounded-by-me list? "list of spanners that have this
column as start/begin point. Only columns that have elements or act as bounds are spaced.")
(elt-property-description 'bracket-thick number? "")
(elt-property-description 'bracket-width number? "")
(elt-property-description 'break-align-symbol symbol? "the index in the spacing table (symbol) of the to be aligned item.")
(elt-property-description 'break-glyph-function procedure? "function taking glyph and break-direction, returning the glyph at a line break")
(elt-property-description 'breakable boolean? "boolean indicating if this is a breakable item (clef, barline, key sig, etc.)")
(elt-property-description 'c0-position integer? "integer indicating the position of central C")

(elt-property-description 'center-element ly-grob? "element which will
be at the center of the group after aligning (when using
Align_interface::center_on_element). ")

(elt-property-description 'collapse-height number? "")
(elt-property-description 'column-space-strength number? "relative strength of space following breakable columns (eg. prefatory matter)")
(elt-property-description 'columns list? "list of paper-columns")
(elt-property-description 'columns list? " list of note-columns.")
(elt-property-description 'columns list? "list of all paper columns")
(elt-property-description 'contains-grace boolean? "Used to widen entries for grace notes.")
(elt-property-description 'control-points list? "List of 4 offsets (number-pairs) controlling the tie shape")
(elt-property-description 'control-points list? "[internal] control points of bezier curve")
(elt-property-description 'damping integer? "amount of beam slope damping should beam slope be damped? 0: no, 1: yes, 100000: horizontal beams ")
(elt-property-description 'damping number? "damping factor.")
(elt-property-description 'dash-length number? "the length of a dash")
(elt-property-description 'dash-period number? "the length of one dash + white space")
(elt-property-description 'dashed number? "[FIXME: use dash-period/dash length; see text-spanner] number representing the length of the dashes.")
(elt-property-description 'de-uglify-parameters list? "list of 3 real constants. They define the valid areas for the middle control points. Used in de_uglyfy. They are a bit empirical.")
(elt-property-description 'default-neutral-direction dir? "Where to go if we're in the middle of the staff")
(elt-property-description 'default-neutral-direction dir? "which
direction to choose if we're in the middle of the staff ")
(elt-property-description 'delta-y number? "amount of ascension")
(elt-property-description 'dependencies list? "list of score-element pointers that indicate who to compute first for certain global passes")
(elt-property-description 'details list? "alist of parameters for the curve shape")
(elt-property-description 'details list? "alist containing contaning a few magic constants.")
(elt-property-description 'dir-forced boolean? "set if direction has been forced; read by Beam.")
(elt-property-description 'dir-function procedure? "function of type (count total)->direction.  Default value: beam-dir-majority, also available: beam-dir-mean, beam-dir-median.")
(elt-property-description 'dir-list list? "list of stem directions, needed for optical spacing correction.")
(elt-property-description 'direction dir? "up or down, left or right?")
(elt-property-description 'direction-source ly-grob? "in case side-relative-direction is set, which element  to get the direction from ")
(elt-property-description 'dot ly-grob? "reference to Dots object.")
(elt-property-description 'dot-count integer? "number of dots")
(elt-property-description 'duration-log integer? "2-log of the notehead duration")
(elt-property-description 'duration-log integer? "log of the duration, ie. 0=whole note, 1 = half note, etc.")
(elt-property-description 'edge-height pair? "a cons that specifies the heights of the vertical egdes '(LEFT-height . RIGHT-height)")
(elt-property-description 'edge-text pair? "a cons that specifies the texts to be set at the edges '(LEFT-text . RIGHT-text)")
(elt-property-description 'elements list? " -- list of items.")
(elt-property-description 'elements list? "list of elements (NoteColumn,
generally) participating in the collision. The
@code{rest-collision} property in @code{elements} is set
to a pointer to the collision")
(elt-property-description 'elements list? "to be aligned elements ")
(elt-property-description 'expand-limit integer? "maximum number of measures expanded in church rests")
(elt-property-description 'extra-extent-X number-pair? "enlarge in X dimension by this much, measured in staff space")
(elt-property-description 'extra-extent-Y number-pair? "see @code{extra-extent-Y}")
(elt-property-description 'extra-offset number-pair? "pair of reals (a cons) forcing an extra offset   before outputting")
(elt-property-description 'extra-space number-pair? "pair of distances")
(elt-property-description 'extra-space number-pair? "(cons LEFT RIGHT)")
(elt-property-description 'extremity-offset-alist list? "an alist (attachment stem-dir*dir slur-dir*dir) -> offset.  The offset adds to the centre of the notehead, or stem.")

(elt-property-description 'extremity-rules list? "an alist (procedure
slur dir) -> attachment to determine the attachment (see above).  If
procedure returns #t, attachment is used.  Otherwise, the next
procedure is tried.")
(elt-property-description 'flag-style string? "")
(elt-property-description 'flag-width-function procedure? "")
(elt-property-description 'font-family symbol? "partial font
definition: music roman braces dynamic math ...")
(elt-property-description 'font-name symbol? "partial font definition:
base name of font file FIXME: should override other partials")
(elt-property-description 'font-point-size number? "partial font definition: exact font size in points FIXME: should override font-relative-size")
(elt-property-description 'font-relative-size number? "partial font definition: the relative size, 0 is style-sheet's normal size, -1 is smaller, +1 is bigger")
(elt-property-description 'font-relative-size integer? "")
(elt-property-description 'font-series symbol? "partial font definition: medium, bold")
(elt-property-description 'font-shape symbol? "partial font definition: upright or italic")

(elt-property-description 'font-style symbol? "a precooked set of font
definitions, eg. finger volta timesig mark script large Large
dynamic")

(elt-property-description 'force-hshift number? "amount of
collision_note_width that overides automatic collision settings. This
is used by @ref{note-collision-interface}")

(elt-property-description 'fraction number-pair? "")
(elt-property-description 'full-size-change boolean? "if set, don't make a change clef smaller.")

(elt-property-description 'glyph symbol? "a string determining what (style) of  glyph is typeset. Valid choices depend on the function that is reading this property. ")
(elt-property-description 'gap number? "Size of a gap in a variable symbol")
(elt-property-description 'glyph-name string? "a name of character within font")

(elt-property-description 'grow-direction dir? "crescendo or decrescendo?")
(elt-property-description 'hair-thickness number? "thickness, measured in stafflinethickness")
(elt-property-description 'heads pair? "pair of element pointers, pointing to the two heads of the  tie. ")
(elt-property-description 'heads list? "list of note heads")
(elt-property-description 'height number? "in staffspace ")
(elt-property-description 'height-quants procedure? "function of type (beam staff-line-thickness) -> list of quants.  Default value: default-beam-dy-quants.
")
(elt-property-description 'horizontal-shift integer? "integer that identifies ranking of note-column for horizontal shifting. This is used by @ref{note-collision-interface}")
(elt-property-description 'horizontal-space number? "amount of space to add after a note (in staff-space)")
(elt-property-description 'ideal-distances list? "(OBJ . (DIST . STRENGTH)) pairs")
(elt-property-description 'interfaces list? "list of symbols indicating the interfaces supported by this object. Is initialized from the @code{meta} field.")
(elt-property-description 'inversion list? " musical-pitch, optional")
(elt-property-description 'items-worth-living list? "list of interesting items. If empty in a particular system, clear that system.")
(elt-property-description 'kern number? "amount of extra white space to add before text.  This is `relative'(?) to the current alignment.")
(elt-property-description 'kern number? "space after a thick line")
(elt-property-description 'left-padding number? "space left of accs")
(elt-property-description 'lengths list? "Stem length given multiplicity of flag")
(elt-property-description 'line-count integer? "Number of staff lines")
(elt-property-description 'line-thickness number? "the thickness[stafflinethickness] of the line")
(elt-property-description 'lookup symbol? "lookup method: 'value for plain text, 'name for character-name")
(elt-property-description 'magnify number? "the magnification factor.  FIXME: doesn't work for feta fonts")
(elt-property-description 'maximum-duration-for-spacing moment? "space as if a duration of this type is available in this measure.")
(elt-property-description 'maximum-rest-count integer? "kill off rests so we don't more than this number left.")
(elt-property-description 'merge-differently-dotted boolean? " Merge noteheads in collisions, even if they have a different number of dots. This normal notation for some types of polyphonic music. The value of this setting is used by @ref{note-collision-interface} ")
(elt-property-description 'minimum-distance number? "minimum distance between notes and rests.")
(elt-property-description 'minimum-distances list? "list of rods (ie. (OBJ . DIST) pairs)")
(elt-property-description 'minimum-extent-X number-pair? "minimum size in X dimension, measured in staff space")
(elt-property-description 'minimum-extent-Y number-pair? "see @code{minimum-extent-Y}")
(elt-property-description 'minimum-length number? "minimum length in staffspace")

(elt-property-description 'minimum-length number? "try to make the
hyphens at least this long. Also works as a scaling parameter for the
length")

;; FIXME.
(elt-property-description 'minimum-space number-pair? "(cons LEFT RIGHT)")
(elt-property-description 'minimum-space number? "minimum distance that the victim should move (after padding)")


(elt-property-description 'minimum-width number? "minimum-width of rest symbol, in staffspace")
(elt-property-description 'molecule-callback procedure? "Function taking graphical element as argument, returning a Scheme encoded Molecule.")
(elt-property-description 'new-accidentals list? "list of (pitch, accidental) pairs")
(elt-property-description 'no-spacing-rods boolean? "read from elements: boolean that makes Separation_item ignore this item (MOVE ME TO ITEM)")
(elt-property-description 'non-default boolean? "not set because of existence of a bar?")
(elt-property-description 'note-width 'number? "unit for horizontal translation, measured in staff-space.")
(elt-property-description 'number-gap number? "")
(elt-property-description 'old-accidentals list? "list of (pitch, accidental) pairs")
(elt-property-description 'origin ly-input-location? "location in input file of the definition")
(elt-property-description 'outer-stem-length-limit number? "catch
suspect beam slopes, set slope to zero if outer stem is lengthened
more than this (in staffspace)")

(elt-property-description 'padding number? "add this much extra space between objects that are next to each other")

(elt-property-description 'parallel-beam boolean? "internal: true if there is a beam just as wide as the bracket ")
(elt-property-description 'pitches list? "list of musical-pitch")
(elt-property-description 'raise number? "height for text to be raised (a negative value lowers the text")
(elt-property-description 'right-padding number? "space right of accs")
(elt-property-description 'right-trim-amount number? "")
(elt-property-description 'script-priority number? "A sorting key that determines in what order a script is within a stack of scripts")
(elt-property-description 'self-alignment-X number? "real number: -1 =
left aligned, 0 = center, 1 right-aligned in X direction.

 Set to an element pointer, if you want that element to be the center.
In this case, the center element should have this object as a
reference point.
")
(elt-property-description 'self-alignment-Y number? "like self-alignment-X but for Y axis")
(elt-property-description 'shortest-playing-duration moment? "duration of the shortest playing in that column.")
(elt-property-description 'shortest-starter-duration moment? "duration of the shortest notes that starts exactly in this column.")
(elt-property-description 'side-relative-direction dir? "if set: get the direction from a different object, and multiply by this.")
(elt-property-description 'side-support list? "the support, a list of score elements")
(elt-property-description 'slope-limit number? "set slope to zero if slope is running away steeper than this.")
(elt-property-description 'space-function procedure? "function of type multiplicity -> real (in staffspace)")
(elt-property-description 'spacing-procedure procedure? "procedure
taking graphical element as argument. This is called after
before-line-breaking-callback, but before the actual line breaking
itself.  Return value is ignored")
(elt-property-description 'stacking-dir dir? "stack contents of elements in which direction ?")
(elt-property-description 'staff-space number? "Amount of line leading relative to global staffspace")
(elt-property-description 'staffline-clearance number? "don't get closer than this to stafflines.")
(elt-property-description 'stem ly-grob? "pointer to the stem object.")
(elt-property-description 'stem ly-grob? "pointer to Stem object")
(elt-property-description 'stem-centered boolean? "Center stems on note heads. Useful for mensural notation")
(elt-property-description 'stem-end-position number? "Where does the stem end (the end is opposite to the support-head")
(elt-property-description 'stem-length number? "length of stem")
(elt-property-description 'stem-shorten list? "shorten stems in forced directions given flag multiplicity")
(elt-property-description 'stem-spacing-correction number? "optical correction amount.")
(elt-property-description 'stems list? "list of stem objects, corresponding to the notes that the arpeggio has to be before.")
(elt-property-description 'stretch-distance number-pair? "pair of distances")
(elt-property-description 'style symbol? "a string determining what style of  glyph is typeset. Valid choices depend on the function that is reading this property. ")
(elt-property-description 'support-head ly-grob? "the note head at
one end of the stem")
(elt-property-description 'text markup? "
Scheme markup text.  It is defined as follows:

@example

TEXT : STRING | (MARKUP SENTENCE)
MARKUP: PROPERTY | ABBREV
SENTENCE: TEXT | SENTENCE TEXT
PROPERTY: (key . value)
ABBREV: rows lines roman music bold italic named super sub text, or any font-style

@end example

So, TEXT is either a string, or a list of which the CAR is a MARKUP.
MARKUP is either a CONS: an element property '(key . value) or a symbol:
a predefined abbreviation for a list of element properties.


The following abbreviations are currently defined:
@table @samp
@item rows
horizontal mode: set all text on one line (default)
@item lines
 vertical mode: set every text on new line
@item roman
 select roman font
@item music
 select feta font
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
@item any font-style
 finger volta timesig mmrest mark script large Large dynamic
@end table
")
(elt-property-description 'thick number? "thickness, in stafflinethickness")
(elt-property-description 'thick-thickness number? "thickness, measured in stafflinethickness")
(elt-property-description 'thickness number? "thickness, measured in stafflinethickness")
(elt-property-description 'thin-kern number? "space after a hair-line")
(elt-property-description 'forced-distance number? "forced distance for an alignment")
(elt-property-description 'threshold number-pair? "(cons MIN MAX), where MIN and MAX are dimensions in staffspace")
(elt-property-description 'transparent boolean? "This is almost the
same as setting molecule-callback to #f, but this retains the
dimensions of this element, which means that you can erase elements
individually. ")
(elt-property-description 'tuplet-bracket-visibility boolean-or-symbol? "
This controls the visibility of the tuplet bracket.
Setting it to false will prevent printing of the
bracket. Setting the property to #'if-no-beam will make it
print only if there is no beam associated with this tuplet bracket.")
(elt-property-description 'tuplet-number-visibility boolean-or-symbol? "
Like @code{tuplet-bracket-visibility}, but for the number.")
(elt-property-description 'type symbol? "one of: line, dashed-line or dotted-line")
(elt-property-description 'vertical-position-quant-function procedure? "
function of type (beam multiplicity dy staff-line-thickness) -> real.  Default value: default-beam-y-quants, also available: beam-traditional-y-quants.
")
(elt-property-description 'visibility-lambda procedure? "a function that takes the break direction and returns a  cons of booleans containing (TRANSPARENT . EMPTY)")
(elt-property-description 'when moment? "when does this column happen?")
(elt-property-description 'word-space number? "elongate left by this much (FIXME: cumbersome semantics)")
(elt-property-description 'x-gap number? "horizontal gap between notehead and tie")
(elt-property-description 'y-free number? "minimal vertical gap between slur and noteheads or stems")
(elt-property-description 'y-position number? "position of left edge")
