
; should include default value?

;;; ::::::: should generate documentation for score elements from here.

(define (elt-property-description symbol type? description)
  (list symbol type? description))
  
(define (lily-interface symbol description props)
  (list symbol
	description
	props
	)
  )

(define (boolean-or-symbol? x) (or boolean? x) (or symbol? x))

(define (uniqued-alist  alist acc)
  (if (null? alist) acc
      (if (assoc (caar alist) acc)
	  (uniqued-alist (cdr alist) acc)
	  (uniqued-alist (cdr alist) (cons (car alist) acc)
  ))))

(define (element-description name . interfaces)
  (let* ((ifs (cons general-element-interface interfaces))
	 (props (map caddr ifs))
	 (prop-typep-pairs (map (lambda (x) (cons (car x) (cadr x)))
					(apply append props)))
	 (syms (map car ifs))
	)
    (list (cons 'separator "\n\n\n")	;easy printing.
	  (cons 'name name)
	  (cons 'interfaces syms)
	  (cons 'interface-descriptions ifs)
	  ; (cons 'interface-descriptions (cadr merged))
	  ;; description of the element itself?
	  (cons 'properties prop-typep-pairs)
  )))


(define general-element-interface
  (lily-interface
   'general-element-interface
   "All elements support this"
   (list
    (elt-property-description 'X-offset-callbacks list? "list of functions, each taking an element and axis argument. The function determine the position relative to this element's parent. The last one in the list is called first")
    (elt-property-description 'Y-offset-callbacks list? "see <code> X-offset-callbacks</code>")
    (elt-property-description 'X-extent-callback procedure? "procedure taking an element and axis argument, returning a number-pair. The return value is the extent of the element.")
    (elt-property-description 'Y-extent-callback procedure? "see <code> X-extent-callback </code>")
    (elt-property-description 'font-relative-size integer? "")
    (elt-property-description 'extra-offset number-pair? "pair of reals (a cons) forcing an extra offset   before outputting")
    (elt-property-description 'interfaces  list? "list of symbols indicating the interfaces supported by this object. Is initialized from the <code>meta</code> field.")
    (elt-property-description 'dependencies list? "list of score-element pointers that indicate who to compute first for certain global passes")
    (elt-property-description 'no-spacing-rods boolean? "read from elements: boolean that makes Separation_item ignore this item (MOVE ME TO ITEM)")
    (elt-property-description 'extra-extent-X number-pair? "enlarge in X dimension by this much, measured in staff space")
    (elt-property-description 'extra-extent-Y number-pair? "see <code>extra-extent-Y</code>")
    (elt-property-description 'minimum-extent-X number-pair? "minimum size in X dimension, measured in staff space")
    (elt-property-description 'minimum-extent-Y number-pair? "see <code>minimum-extent-Y</code>")
    (elt-property-description 'origin ly-input-location? "location in input file of the definition")
    (elt-property-description 'transparent boolean? "This is almost the
same as setting molecule-callback to #f, but this retains the
dimensions of this element, which means that you can erase elements
individually. ")
    (elt-property-description 'molecule-callback procedure? "Function taking graphical element as argument, returning a Scheme encoded Molecule

This function can be called more than once (for instance once for
computing dimensions, and once for computing the output).  Therefore,
this function should have no side-effects on its argument.
Caching of computed values is permissible, and generally useful, though.

") 
    ))
  )

(define beam-interface
  (lily-interface
   'beam-interface
   "A beam. "
   (list
    (elt-property-description 'y-position number? "position of left edge")
    (elt-property-description 'height number? "dy")
    (elt-property-description 'flag-width-function procedure? "")
    (elt-property-description 'damping integer? "amount of beam slope damping should beam slope be damped? 0: no, 1: yes, 100000: horizontal beams ")
    (elt-property-description 'default-neutral-direction dir? "which
direction to choose if we're in the middle of the staff ")
    (elt-property-description 'thickness number? "weight of beams, in staffspace")
    (elt-property-description 'space-function procedure? "function of type multiplicity -> real (in staffspace)")
    (elt-property-description 'beamed-stem-shorten number? "")
    (elt-property-description 'height-quants number? "")
    (elt-property-description 'vertical-position-quant-function procedure? "")
    (elt-property-description 'dir-function procedure? "")
    (elt-property-description 'damping number? "damping factor.")
    (elt-property-description 'outer-stem-length-limit number? "catch
suspect beam slopes, set slope to zero if outer stem is lengthened
more than this (in staffspace)")
    (elt-property-description 'slope-limit number? "set slope to zero if slope is running away steeper than this.")
    )
))



;;;;;;;;;;;;;;;;;;;;

(define clef-interface
  (lily-interface
   'clef-interface
   "A clef sign"
   (list
    (elt-property-description 'non-default boolean? "not set because of existence of a bar?")
    (elt-property-description 'change boolean? "is this a change clef (smaller size)?")
    (elt-property-description 'glyph string? "a string determining what glyph is typeset")
    ))
  )

(define axis-group-interface
  (lily-interface
   'axis-group-interface
   "a group of coupled elements"
   (list
    (elt-property-description 'axes list? "list of axis (number) in which this group works")
   )))

(define note-column-interface
  (lily-interface
   'note-column-interface
   "Stem and noteheads combined"
   (list
    (elt-property-description 'horizontal-shift integer? "integer that identifies ranking of note-column for horizontal shifting.")
    (elt-property-description 'force-hshift number? "amount of collision_note_width that overides automatic collision settings.")
    (elt-property-description 'merge-differently-dotted boolean? "merge black noteheads with differing dot count in collisions.
<p>
Merge noteheads in collisions, even if they have a different number of
dots. This normal notation for polyphonic guitar music.

")
    ))
  )

(define stem-interface
  (lily-interface
   'stem-interface
   "A stem"
   (list
    (elt-property-description 'thickness number? "thickness, measured in stafflinethickness")
    (elt-property-description 'beamed-lengths list? "list of stem lengths given beam multiplicity ")
    (elt-property-description 'beamed-minimum-lengths list? "list of minimum stem lengths given beam multiplicity")
    (elt-property-description 'stem-centered boolean? "Center stems on note heads. Useful for mensural notation")
    (elt-property-description 'lengths list? "Stem length given multiplicity of flag")
    (elt-property-description 'beam ly-element? "pointer to the beam, if applicable")
    (elt-property-description 'stem-shorten list? "shorten stems in forced directions given flag multiplicity")
    (elt-property-description 'duration-log integer? "log of the duration, ie. 0=whole note, 1 = half note, etc.")
    (elt-property-description 'beaming number-pair? "number of beams extending to left and right")
    (elt-property-description 'default-neutral-direction dir? "Where to go if we're in the middle of the staff")
    (elt-property-description 'stem-end-position number? "Where does the stem end (the end is opposite to the support-head")
    (elt-property-description 'support-head ly-element? "the note head at
one end of the stem")
    (elt-property-description 'heads list? "list of note heads")
    (elt-property-description 'direction dir? "up or down")
    (elt-property-description 'stem-length number? "length of stem")
    (elt-property-description 'style string? "") ; symbol!?
    (elt-property-description 'flag-style string? "") ; symbol!?
    (elt-property-description 'dir-forced boolean? "set if direction has been forced; read by Beam.")
    )))


(define slur-interface
  (lily-interface
   'slur-interface
   "A slur"
   (list
    (elt-property-description 'de-uglify-parameters list? "list of 3 real constants. They define the valid areas for the middle control points. Used in de_uglyfy. They are a bit empirical.")
    (elt-property-description 'details list? "alist containing contaning a few magic constants.")
    (elt-property-description 'attachment pair? "cons of symbols, '(LEFT-TYPE . RIGHT-TYPE), where both types may be alongside-stem, stem, head or loose-end")
    (elt-property-description 'direction dir? "up or down?")
   (elt-property-description 'attachment-offset pair? "cons of offsets, '(LEFT-offset . RIGHT-offset).  This offset is added to the attachments to prevent ugly slurs.")
     (elt-property-description 'beautiful number? "number that dictates when a slur should be de-uglyfied.  It correlates with the enclosed area between noteheads and slurs.  A value of 0.1 yields only undisturbed slurs, a value of 5 will tolerate quite high blown slurs.")
     (elt-property-description 'y-free number? "minimal vertical gap between slur and noteheads or stems")
     (elt-property-description 'control-points list? "[internal] control points of bezier curve")
     (elt-property-description 'extremity-rules  list? "an alist (procedure slur dir) -> attachment to determine the attachment (see above).  If procedure returns #t, attachment is used.  Otherwise, the next procedure is tried.")
     (elt-property-description 'extremity-offset-alist list? "an alist (attachment stem-dir*dir slur-dir*dir) -> offset.  The offset adds to the centre of the notehead, or stem.")
     (elt-property-description 'thickness list? "The thickness[stafflinethickness] of slur in the centre.")
     (elt-property-description 'dashed number? "[FIXME: use dash-period/dash length; see text-spanner] number representing the length of the dashes.")

    )
   )
  )

(define side-position-interface
  (lily-interface
   'side-position-interface
   "Position a victim object (this one) next to other objects (the support)."
   (list
   (elt-property-description 'side-support list? "the support, a list of score elements")
   (elt-property-description 'direction-source ly-element? "in case side-relative-direction is set, which element  to get the direction from ")
    (elt-property-description 'direction dir? "where to put the victim object (left or right?)")
    (elt-property-description 'side-relative-direction dir? "if set: get the direction from a different object, and multiply by this.")
    (elt-property-description 'minimum-space number? "minimum distance that the victim should move (after padding)")
    (elt-property-description 'padding number? "add this much extra space between victim and support")
    (elt-property-description 'self-alignment-X number? "real number: -1 =
left aligned, 0 = center, 1 right-aligned in X direction. <p> Set to
an element pointer, if you want that element to be the center.  In
this case, the center element should have this object as a reference
point.
")
    (elt-property-description 'self-alignment-Y number? "like self-alignment-X but for Y axis")
    
    )
  ))

(define accidentals-interface
  (lily-interface
   'accidentals-interface
   "Accidentals"
   (list
    (elt-property-description 'left-padding number? "space left of accs")
    (elt-property-description 'right-padding number? "space right of accs")     
    )
   ))

(define line-of-score-interface
  (lily-interface
   'line-of-score-interface
   "Super element, parent of all:
<p>
   The columns of a score that form one line.  The toplevel element.
   Any element has a Line_of_score as both X and Y reference
   point. The Paper_score contains one element of this type. Control
   enters the Score_element dependency calculation from this single
   Line_of_score object."
   (list
    (elt-property-description 'between-system-string string? "string
 to dump between two systems. Useful for forcing pagebreaks")
    (elt-property-description 'spacing-procedure procedure? "procedure taking
graphical element as argument. This is called after before-line-breaking-callback, but before the actual line breaking itself.  Return value is ignored")
    (elt-property-description 'before-line-breaking-callback procedure?
			  "Procedure taking graphical element as argument.
This procedure is called (using dependency resolution) before line breaking, but after generating discretionary items. Return value is ignored")
    (elt-property-description 'after-line-breaking-callback procedure?
			  "Procedure taking graphical element as argument.
This procedure is called (using dependency resolution) after line breaking. Return value is ignored")
    (elt-property-description 'all-elements list? "list of all score elements in this line. Needed for protecting elements from GC.")
    (elt-property-description 'columns list? "list of all paper columns")
    )))

(define note-head-interface
  (lily-interface
   'note-head-interface
   "Note head"
   (list
    (elt-property-description 'style symbol? "symbol that sets note head style")
    )
   ))

(define note-name-interface
  (lily-interface
   'note-name-interface
   "Note name"
   (list
    (elt-property-description 'style symbol? "symbol that sets note name style")
    )
   ))


(define rhythmic-head-interface
  (lily-interface
   'rhythmic-head-interface
   "Note head or rest"
   (list
    (elt-property-description 'dot ly-element? "reference to Dots object.")
    (elt-property-description 'stem ly-element? "pointer to Stem object")
    (elt-property-description 'duration-log integer? "2-log of the notehead duration")
    )))

(define rest-interface
  (lily-interface
   'rest-interface
   "a rest"
   (list
    (elt-property-description 'style string? "string specifying glyph style"))))

(define tuplet-bracket-interface
  (lily-interface
   'tuplet-bracket-interface
   "A bracket with a number in the middle, used for tuplets." 
   (list
    (elt-property-description 'beams list? "list of beam ptrs.")
    (elt-property-description 'columns list? " list of note-columns.")
    (elt-property-description 'number-gap number? "")
    (elt-property-description 'delta-y number? "amount of ascension")
    (elt-property-description 'tuplet-bracket-visibility boolean-or-symbol? "
This controls the visibility of the tuplet bracket.
Setting it to false will prevent printing of the
bracket. Setting the property to #'if-no-beam will make it
print only if there is no beam associated with this tuplet bracket.")
    (elt-property-description 'tuplet-number-visibility boolean-or-symbol? "
Like <code>tuplet-bracket-visibility</code>, but for the number.")
    (elt-property-description 'parallel-beam boolean? "internal: true if there is a beam just as wide as the bracket ")
    (elt-property-description 'thick number? "thickness, in stafflinethickness")
    )
))


(define align-interface
  (lily-interface
   'align-interface
   " Order elements top to bottom/left to right/right to left etc."
   (list
    (elt-property-description 'stacking-dir  dir? "stack contents of elements in which direction ?")
    (elt-property-description 'align-dir  dir? "Which side to align? -1: left side, 0: around center of width, 1: right side")
    (elt-property-description 'threshold  number-pair? "(cons MIN MAX), where MIN and MAX are dimensions in staffspace")
    (elt-property-description 'alignment-done  boolean? "boolean to administrate whether we've done the alignment already (to ensure that the process is done only once)")
    (elt-property-description 'center-element ly-element? "element which will be at the
center of the group after aligning (when using
Align_interface::center_on_element). ")
    (elt-property-description 'elements  list? "to be aligned elements ")
    (elt-property-description 'axes  list? "list of axis numbers. Should contain only one number.")
    )))    

(define aligned-interface
  (lily-interface
   'aligned-interface
   "read by align-interface"
   (list
    (elt-property-description 'minimum-space number-pair? "(cons LEFT RIGHT)")
    (elt-property-description 'extra-space number-pair? "(cons LEFT RIGHT)")
    )))

(define break-aligned-interface
  (lily-interface
   'break-aligned-interface
   "Items that are aligned in prefatory matter"
   (list
    (elt-property-description 'break-align-symbol symbol? "the index in the spacing table (symbol) of the to be aligned item.")
    (elt-property-description 'visibility-lambda procedure? "a function that takes the break direction and returns a  cons of booleans containing (TRANSPARENT . EMPTY)")
    (elt-property-description 'breakable boolean? "boolean indicating if this is a breakable item (clef, barline, key sig, etc.)")
    )))

(define chord-name-interface
  (lily-interface
   'chord-name-interface
   "generate a chord name"
   (list
    (elt-property-description 'pitches list? "list of musical-pitch")
    (elt-property-description 'inversion list? " musical-pitch, optional")
    (elt-property-description 'bass list? " musical-pitch, optional")
   )))

(define time-signature-interface
  (lily-interface
   'time-signature-interface
   "A time signature, in different styles"
   (list
    (elt-property-description 'fraction number-pair? "")
    (elt-property-description 'style string? "")
    )))

(define bar-line-interface
  (lily-interface
   'bar-line-interface
   "Bar line"
   (list
    (elt-property-description 'barsize-procedure procedure? "how to compute the size of a bar line")
    (elt-property-description 'kern number? "space after a thick line")
    (elt-property-description 'thin-kern number? "space after a hair-line")
    (elt-property-description 'hair-thickness number? "thickness, measured in stafflinethickness")
    (elt-property-description 'thick-thickness number? "thickness, measured in stafflinethickness")
    (elt-property-description 'glyph string? "what kind barline? A concatenation of |, : and .")
    (elt-property-description 'bar-size number? "")
    (elt-property-description 'break-glyph-function procedure? "function taking glyph and break-direction, returning the glyph at a line break")
   )))




(define hairpin-interface
  (lily-interface
   'hairpin-interface
   "hairpin crescendo"
   (list
    (elt-property-description 'grow-direction dir? "crescendo or decrescendo?")
    (elt-property-description 'thickness number? "thickness, measured in stafflinethickness")
    (elt-property-description 'height number? "height, measured in staffspace in ")
    )))

(define arpeggio-interface
  (lily-interface
   'arpeggio-interface
   "arpeggio"
   (list
    (elt-property-description 'stems list? "list of stem objects, corresponding to the notes that the arpeggio has to be before.")
    )
   )
  )

(define note-collision-interface
  (lily-interface
   'note-collision-interface
   "note collision"
   (list
    (elt-property-description 'note-width 'number? "unit for horizontal translation, measured in staff-space.")
    )   )  )


(define custos-interface
  (lily-interface
   'custos-interface
   "A custos is a staff context symbol that appears at the end of a
  staff line with monophonic musical contents (i.e. with a single
  voice).  It anticipates the pitch of the first note of the following
  line and thus helps the player or singer to manage line breaks
  during performance, thus enhancing readability of a score.

  Custodes were frequently used in music notation until the 16th
  century.  There were different appearences for different notation
  styles.  Nowadays, they have survived only in special forms of
  musical notation such as via the editio vaticana dating back to the
  beginning of the 20th century.

[TODO: add to glossary]"

   (list
    (elt-property-description 'style string? "a string determining what glyph is 
typeset. Current choices are mensural, 
hufnagel, vaticana and medicaea [TODO: should use symbol] ")
    ))
  )



(define dot-interface
  (lily-interface
   'dots-interface
   "The dots to go with a notehead/rest.  A separate interface, since they
  are a party in collision resolution."
   (list
    (elt-property-description 'direction dir? "Direction to handle staff-line collisions in.")
    (elt-property-description 'dot-count integer? "number of dots")
    )))

(define font-interface
  (lily-interface
   'font-interface
   "Any symbol that is typeset through fixed sets of glyphs (ie. fonts)"
   (list
    (elt-property-description 'font-style symbol? "a precooked set of font definitions, eg. finger volta timesig mark script large Large dynamic")
    (elt-property-description 'font-series symbol? "partial font definition: medium, bold")
    (elt-property-description 'font-shape symbol?  "partial font definition: upright or italic")
    (elt-property-description 'font-family symbol? "partial font definition: music roman braces dynamic math ...")
    (elt-property-description 'font-name symbol? "partial font definition: base name of font file FIXME: should override other partials")
    (elt-property-description 'font-point-size number? "partial font definition: exact font size in points FIXME: should override font-relative-size")
    (elt-property-description 'font-relative-size number? "partial font definition: the relative size, 0 is style-sheet's normal size, -1 is smaller, +1 is bigger")
    )))


(define text-interface
  (lily-interface
   'text-interface
   "A scheme markup text"
   (list
    (elt-property-description 'text (lambda (x) (or (string? x) (list? x))) "
Scheme markup text.  It is defined as follows:
<p>

TEXT : STRING | (MARKUP SENTENCE)<br>
MARKUP: PROPERTY | ABBREV<br>
SENTENCE: TEXT | SENTENCE TEXT<br>
PROPERTY: (key . value)<br>
ABBREV: rows lines roman music bold italic named super sub text, or any font-style
<p>

So, TEXT is either a string, or a list of which the CAR is a MARKUP.
MARKUP is either a CONS: an element property '(key . value) or a symbol:
a predefined abbreviation for a list of element properties.
<p>

The following abbreviations are currently defined:
<dl>
<dt>rows<dd> horizontal mode: set all text on one line (default)
<dt>lines<dd> vertical mode: set every text on new line
<dt>roman<dd> select roman font
<dt>music<dd> select feta font
<dt>bold<dd> select bold series
<dt>italic<dd> select italic shape
<dt>named<dd> lookup by character name
<dt>text<dd> plain text lookup (by character value)
<dt>super<dd> superscript
<dt>sub<dd> subscript
<dt> any font-style<dd> finger volta timesig mmrest mark script large Large dynamic
</dl>
" )
    ;; Should move this somewhere else?  
    (elt-property-description 'align number? "the alignment of the text, 0 is horizontal, 1 is vertical")
    (elt-property-description 'lookup symbol? "lookup method: 'value for plain text, 'name for character-name")
    (elt-property-description 'raise number? "height for text to be raised (a negative value lowers the text")
    (elt-property-description 'kern number? "amount of extra white space to add before text.  This is `relative'(?) to the current alignment.")
    (elt-property-description 'magnify number? "the magnification factor.  FIXME: doesn't work for feta fonts")
    )))

(define dot-column-interface
  (lily-interface
   'dot-column-interface
   "Interface that groups dots so they form a column"
   (list
    )))

(define dynamic-interface
  (lily-interface
   'dynamic-interface
   "Any kind of loudness sign"
   '()
    ))


(define finger-interface
  (lily-interface
   'finger-interface
   "A fingering instruction"
   '()
    ))

(define separation-spanner-interface
  (lily-interface
   'separation-spanner-interface
   "Spanner that containing <code>separation-item-interface</code> elements to calculate rods"
   '()
  ))
(define text-script-interface
  (lily-interface
   'text-script-interface
   "Any text script"
   '()
    ))

(define grace-alignment-interface
  (lily-interface
   'grace-alignment-interface
   "put grace notes in line"
   (list
    (elt-property-description 'horizontal-space number? "amount of space to add after a note (in staff-space)")
    )
   ))

(define hara-kiri-group-interface
  (lily-interface
   'hara-kiri-group-interface
   "  As Vertical_group_spanner, but keep track of interesting items.  If
  we don't contain any interesting items after linebreaking, then
  gracefully commit suicide.  Objective: don't disgrace Lily by
  typesetting empty lines in orchestral scores."
   (list
    (elt-property-description 'items-worth-living list? "list of interesting items. If empty in a particular system, clear that system.")


    )))

(define lyric-hyphen-interface
  (lily-interface
   'lyric-hyphen-interface
   "A centred hyphen is a simple line between lyrics used to divide
syllables.   The length of the hyphen line should stretch based on the
  size of the gap between syllables."
   (list
    
    (elt-property-description 'thickness number? "thickness of line (in stafflinethickness)")
    (elt-property-description 'height number? "vertical offset  (in staffspace)")

    (elt-property-description 'minimum-length number? "try to make the hyphens at least this long. Also works as a scaling parameter for the length")
    (elt-property-description 'word-space number? "elongate left by this much (FIXME: cumbersome semantics)")
    )))

(define key-signature-interface
  (lily-interface
   'key-signature-interface
   "A group of  accidentals."
   (list
    (elt-property-description 'c0-position  integer? "integer indicating the position of central C")
    (elt-property-description 'old-accidentals  list? "list of (pitch, accidental) pairs")
    (elt-property-description 'new-accidentals  list? "list of (pitch, accidental) pairs")
    )))

(define lyric-extender-interface
  (lily-interface
   'lyric-extender-interface
   "The extender is a simple line at the baseline of the lyric
  that helps show the length of a melissima (tied/slurred note)."
   (list
    (elt-property-description 'word-space  number? "")
    (elt-property-description 'height  number? "in stafflinethickness")
    (elt-property-description 'right-trim-amount  number? "")
    )))


(define lyric-syllable-interface
  (lily-interface
   'lyric-syllable-interface
   "a single piece of lyrics"
   (list
    (elt-property-description 'word-space  number? "")
    )))


(define mark-interface
  (lily-interface
   'mark-interface
   "a rehearsal mark"
   (list
    )))

(define multi-measure-rest-interface
  (lily-interface
   'multi-measure-rest-interface
   "A rest that spans a whole number of measures.  For typesetting the
numbers, fields from font-interface may be used. 
"
   (list
    
    (elt-property-description 'columns  list? "list of paper-columns")
    (elt-property-description 'expand-limit  integer? "maximum number of measures expanded in church rests")
    (elt-property-description 'minimum-width number? "minimum-width of rest symbol, in staffspace")
    (elt-property-description 'padding  number? "padding between number and rest. Measured in staffspace.")
    )))

(define paper-column-interface
  (lily-interface
   'paper-column-interface
   ""
   (list
    (elt-property-description 'column-space-strength number? "relative strength of space following breakable columns (eg. prefatory matter)")
    (elt-property-description 'before-musical-spacing-factor number?
"space before musical columns (eg. taken by accidentals) get this much
stretched when they follow a musical column, in absence of grace
notes.  0.0 means no extra space (accidentals are ignored)")
    (elt-property-description 'stem-spacing-correction number? "optical correction amount.")
    (elt-property-description 'before-grace-spacing-factor number? " stretch space this much if there are grace notes before the column")
    (elt-property-description 'when moment? "when does this column happen?")
    (elt-property-description 'bounded-by-me list? "list of spanners that have this
column as start/begin point. Only columns that have elements or act as bounds are spaced.")
    (elt-property-description 'dir-list  list? "list of stem directions")
    (elt-property-description 'shortest-playing-duration  moment? "duration of the shortest playing in that column.")
    (elt-property-description 'shortest-starter-duration  moment? "duration of the shortest notes that starts exactly in this column.")
    (elt-property-description 'contains-grace  boolean? "Used to widen entries for grace notes.")
    (elt-property-description 'extra-space  number-pair? "pair of distances")
    (elt-property-description 'stretch-distance number-pair? "pair of distances")
    )))

(define spaceable-element-interface
  (lily-interface
   'spaceable-element-interface
   "An element (generally a Paper_column) that takes part in the
spacing problem. "
   (list
     (elt-property-description 'minimum-distances list? "list of rods (ie. (OBJ . DIST) pairs)")
     (elt-property-description 'ideal-distances  list? "(OBJ . (DIST . STRENGTH)) pairs")
     (elt-property-description 'dir-list list? "list of stem directions, needed for optical spacing correction.")
     )))

(define rest-collision-interface
  (lily-interface
   'rest-collision-interface
   "Move around ordinary rests (not multi-measure-rests) to avoid
conflicts."
   (list
    (elt-property-description 'maximum-rest-count integer? "kill off rests so we don't more than this number left.")
    (elt-property-description 'minimum-distance number? "minimum distance between notes and rests.")
    (elt-property-description 'elements list? "list of elements (NoteColumn,
generally) participating in the collision. The
<code>rest-collision</code> property in <code>elements</code> is set
to a pointer to the collision")
    )))

(define script-interface
  (lily-interface
   'script-interface
   ""
   (list
    (elt-property-description 'script-priority number? "A sorting key that determines in what order a script is within a stack of scripts")
    )))

(define script-column-interface
  (lily-interface
   'script-column-interface
   "An interface that sorts scripts according to their <code>script-priority</code>"
   (list )))


(define spacing-spanner-interface
  (lily-interface
   'spacing-spanner-interface
   ""
   (list
    (elt-property-description 'maximum-duration-for-spacing moment? "space as if a duration of this type is available in this measure.")
    (elt-property-description 'arithmetic-basicspace number? "The space taken by a note is determined by the formula 

   SPACE = arithmetic_multiplier * ( C + log2 (TIME) ))

where TIME is the amount of time a note occupies.  The value of C is
chosen such that the smallest space within a measure is
arithmetic_basicspace:

  C = arithmetic_basicspace - log2 (mininum (SHORTEST, 1/8)) 

The smallest space is the one following the shortest note in the
measure, or the space following a hypothetical 1/8 note.  Typically
arithmetic_basicspace is set to a value so that the shortest note
takes about two noteheads of space (ie, is followed by a notehead of
space):

   2*quartwidth = arithmetic_multiplier * ( C + log2 (SHORTEST) ))

   { using: C = arithmetic_basicspace - log2 (mininum (SHORTEST, 1/8)) }
   { assuming: SHORTEST <= 1/8 }

               = arithmetic_multiplier *
	       ( arithmetic_basicspace - log2 (SHORTEST) + log2 (SHORTEST) )

               = arithmetic_multiplier * arithmetic_basicspace

   { choose: arithmetic_multiplier = 1.0*quartwidth (why?)}

               = quartwidth * arithmetic_basicspace

   =>	       

   arithmetic_basicspace = 2/1 = 2

If you want to space your music wider, use something like:

   arithmetic_basicspace = 4.;

")
    (elt-property-description 'arithmetic-multiplier number? "see arithmetic-basicspace")    
    
    )))

(define staff-symbol-interface
  (lily-interface
   'staff-symbol-interface
   "This spanner draws the lines of a staff.  The middle line is
position 0."
   (list
    (elt-property-description 'staff-space number? "Amount of line leading relative to global staffspace")
    (elt-property-description 'line-count integer? "Number of staff lines")
    )))

(define stem-tremolo-interface
  (lily-interface
   'stem-tremolo-interface
   ""
   (list
    (elt-property-description 'stem ly-element? "pointer to the stem object.")
    (elt-property-description 'beam-width number? "width of the tremolo sign")
    (elt-property-description 'beam-thickness number? "thickness, measured in staffspace")
    (elt-property-description 'beam-space-function procedure? "function returning space given multiplicity")
    )))

(define separation-item-interface
  (lily-interface
   'separation-item-interface
   "Item that computes widths to generate spacing rods.
<p>
Calc dimensions for the Separating_group_spanner; this has to be
   an item to get dependencies correct.  It can't be an element_group
   since these usually are in a different X_group
"
   (list
    (elt-property-description 'elements list? " -- list of items.")
     )))

(define sustain-pedal-interface
  (lily-interface
   'sustain-pedal-interface
   ""
   (list
    )))
(define system-start-delimiter
  (lily-interface
   'system-start-delimiter
   ""
   (list
    (elt-property-description 'collapse-height number? "")
    (elt-property-description 'thickness number? "thickness, measured in stafflinethickness")

    ; Should collapse into (bracket . ((height . ) ... ))
    ;
    (elt-property-description 'arch-height number? "")
    (elt-property-description 'arch-angle number? "")
    (elt-property-description 'arch-thick number? "")
    (elt-property-description 'arch-width number? "")
    (elt-property-description 'bracket-thick number? "")
    (elt-property-description 'bracket-width number? "")
    (elt-property-description 'glyph symbol? "bar-line, bracket or brace")
    )))

(define text-spanner-interface
  (lily-interface
   'text-spanner-interface
   "generic text spanner"
   (list
    (elt-property-description 'dash-period  number? "the length of one dash + white space")
    (elt-property-description 'dash-length number? "the length of a dash")
    (elt-property-description 'line-thickness number? "the thickness[stafflinethickness] of the line")
    (elt-property-description 'edge-height pair? "a cons that specifies the heights of the vertical egdes '(LEFT-height . RIGHT-height)")
    (elt-property-description 'edge-text pair? "a cons that specifies the texts to be set at the edges '(LEFT-text . RIGHT-text)")
    (elt-property-description 'type string? "one of: line, dashed-line or dotted-line") ; SYMBOL!!?    
    )
))

(define text-script-interface
  (lily-interface
   'text-script-interface
   ""
   (list
    
    )))


(define tie-interface
  (lily-interface
   'tie-interface
   "A tie connecting two noteheads."
   (list
    (elt-property-description 'staffline-clearance number? "don't get closer than this to stafflines.")
    (elt-property-description 'control-points list? "List of 4 offsets (number-pairs) controlling the tie shape")
    (elt-property-description 'heads pair? "pair of element pointers, pointing to the two heads of the  tie. ")
    (elt-property-description 'details list? "alist of parameters for the curve shape")
    (elt-property-description 'thickness number? "thickness, measured in stafflinethickness")
    (elt-property-description 'x-gap number? "horizontal gap between notehead and tie")
    (elt-property-description 'direction dir? "up or down?")    
    (elt-property-description 'minimum-length number? "minimum length in staffspace")
    )))



(define tie-column-interface
  (lily-interface
   'tie-column-interface
   "that sets tie directions in a tied chord"
   (list
    (elt-property-description 'direction dir? "Forced direction for all ties") 
    )))

(define volta-bracket-interface
  (lily-interface
   'volta-bracket-interface
   "Volta bracket with number"
   (list
    (elt-property-description 'bars  list? "list of barline ptrs.")
    (elt-property-description 'thickness  number? "thickness, measured in stafflinethickness")
    (elt-property-description 'height  number? "in staffspace ")
    )))

(define span-bar-interface
  (lily-interface
   'span-bar-interface
   ""
   (list
    )))

