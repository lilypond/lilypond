
; should include default value?

;;; ::::::: should generate documentation for score elements from here.

(define (property-description symbol type? description)
  (list symbol type? description))
  
(define (lily-interface symbol description props)
  (list symbol
	description
	props
	)
  )


(define (merge-interfaces ifs)
   (list
    (apply append (map car ifs))
    (apply append (map cadr ifs))
    (apply append (map caddr ifs))
  ))

(define (uniqued-alist  alist acc)
  (if (null? alist) acc
      (if (assoc (caar alist) acc)
	  (uniqued-alist (cdr alist) acc)
	  (uniqued-alist (cdr alist) (cons (car alist) acc)
  ))))

(define (element-description name . interfaces)
  (let* ((ifs (cons general-element-interface interfaces))
	 (props (map caddr ifs))
	 (syms (map car ifs))
	)
    (list (cons 'separator "\n\n\n")	;easy printing.
	  (cons 'name name)
	  (cons 'interfaces syms)
	  (cons 'interface-descriptions ifs)
	  ; (cons 'interface-descriptions (cadr merged))
	  ;; description of the element itself?
	  (cons 'properties (apply append props))
  )))


(define general-element-interface
  (lily-interface
   'general-element-interface
   "All elements support this"
   (list
    (property-description 'X-offset-callbacks list? "list of functions, each taking an element and axis argument. The function determine the position relative to this element's parent. The last one in the list is called first")
    (property-description 'Y-offset-callbacks list? "see <code> X-offset-callbacks</code>")
    (property-description 'X-extent-callback procedure? "procedure taking an element and axis argument, returning a number-pair. The return value is the extent of the element.")
    (property-description 'Y-extent-callback procedure? "see <code> X-extent-callback </code>")
    (property-description 'font-size integer? "")
    (property-description 'extra-offset number-pair? "pair of reals (a cons) forcing an extra offset   before outputting")
    (property-description 'interfaces  list? "list of symbols indicating the interfaces supported by this object. Is initialized from the <code>meta</code> field.")
    (property-description 'dependencies list? "list of score-element pointers that indicate who to compute first for certain global passes")
    (property-description 'no-spacing-rods boolean? "read from elements: boolean that makes Separation_item ignore this item (MOVE ME TO ITEM)")
    (property-description 'extra-extent-X number-pair? "enlarge in X dimension by this much, measured in staff space")
    (property-description 'extra-extent-Y number-pair? "see <code>extra-extent-Y</code>")
    (property-description 'minimum-extent-X number-pair? "minimum size in X dimension, measured in staff space")
    (property-description 'minimum-extent-Y number-pair? "see <code>minimum-extent-Y</code>")
    (property-description 'origin ly-input-location? "location in input file of the definition")
    (property-description 'transparent boolean? "This is almost the
same as setting molecule-callback to #f, but this retains the
dimensions of this element, which means that you can erase elements
individually. ")
    (property-description 'molecule-callback procedure? "Function taking graphical element as argument, returning a Scheme encoded Molecule") 
    ))
  )

(define beam-interface
  (lily-interface
   'beam-interface
   "A beam. "
   (list
    (property-description 'y-position number? "position of left edge")
    (property-description 'height number? "dy")
    (property-description 'flag-width-function procedure? "")
    (property-description 'damping integer? "amount of beam slope damping should beam slope be damped? 0: no, 1: yes, 100000: horizontal beams ")
    (property-description 'default-neutral-direction dir? "which
direction to choose if we're in the middle of the staff ")
    (property-description 'thickness number? "weight of beams, in staffspace")
    (property-description 'space-function procedure? "function of type multiplicity -> real (in staffspace)")
    (property-description 'beamed-stem-shorten number? "")
    (property-description 'height-quants number? "")
    (property-description 'vertical-position-quant-function procedure? "")
    (property-description 'dir-function procedure? "")
    (property-description 'damping number? "damping factor.")
    (property-description 'outer-stem-length-limit number? "catch
suspect beam slopes, set slope to zero if outer stem is lengthened
more than this (in staffspace)")
    (property-description 'slope-limit number? "set slope to zero if slope is running away steeper than this.")
    )
))



;;;;;;;;;;;;;;;;;;;;

(define clef-interface
  (lily-interface
   'clef-interface
   "A clef sign"
   (list
    (property-description 'non-default boolean? "not set because of existence of a bar?")
    (property-description 'change boolean? "is this a change clef (smaller size)?")
    (property-description 'glyph string? "a string determining what glyph is typeset")
    ))
  )

(define axis-group-interface
  (lily-interface
   'axis-group-interface
   "a group of coupled elements"
   (list
    (property-description 'axes list? "list of axis (number) in which this group works")
   )))

(define note-column-interface
  (lily-interface
   'note-column-interface
   "Stem and noteheads combined"
   (list
    (property-description 'horizontal-shift integer? "integer that identifies ranking of note-column for horizontal shifting.")
    (property-description 'force-hshift number? "amount of collision_note_width that overides automatic collision settings.")
    (property-description 'merge-differently-dotted boolean? "merge black noteheads with differing dot count in collisions.")
    ))
  )

(define stem-interface
  (lily-interface
   'stem-interface
   "A stem"
   (list
    (property-description 'thickness number? "thickness, measured in stafflinethickness")
    (property-description 'beamed-lengths list? "list of stem lengths given beam multiplicity ")
    (property-description 'beamed-minimum-lengths list? "list of minimum stem lengths given beam multiplicity")
    (property-description 'stem-centered boolean? "Center stems on note heads. Useful for mensural notation")
    (property-description 'lengths list? "Stem length given multiplicity of flag")
    (property-description 'beam ly-element? "pointer to the beam, if applicable")
    (property-description 'stem-shorten list? "shorten stems in forced directions given flag multiplicity")
    (property-description 'duration-log integer? "log of the duration, ie. 0=whole note, 1 = half note, etc.")
    (property-description 'beaming number-pair? "number of beams extending to left and right")
    (property-description 'default-neutral-direction dir? "Where to go if we're in the middle of the staff")
    (property-description 'stem-end-position number? "Where does the stem end (the end is opposite to the support-head")
    (property-description 'support-head ly-element? "the note head at
one end of the stem")
    (property-description 'heads list? "list of note heads")
    (property-description 'direction dir? "up or down")
    (property-description 'stem-length number? "length of stem")
    (property-description 'style string? "") ; symbol!?
    (property-description 'flag-style string? "") ; symbol!?
    (property-description 'dir-forced boolean? "set if direction has been forced; read by Beam.")
    )))


(define slur-interface
  (lily-interface
   'slur-interface
   "A slur"
   (list
    (property-description 'de-uglify-parameters list? "list of 3 real constants. They define the valid areas for the middle control points. Used in de_uglyfy. They are a bit empirical.")
    (property-description 'details list? "alist containing contaning a few magic constants.")
    (property-description 'attachment pair? "cons of symbols, '(LEFT-TYPE . RIGHT-TYPE), where both types may be alongside-stem, stem, head or loose-end")
    (property-description 'direction dir? "up or down?")
   (property-description 'attachment-offset pair? "cons of offsets, '(LEFT-offset . RIGHT-offset).  This offset is added to the attachments to prevent ugly slurs.")
     (property-description 'beautiful number? "number that dictates when a slur should be de-uglyfied.  It correlates with the enclosed area between noteheads and slurs.  A value of 0.1 yields only undisturbed slurs, a value of 5 will tolerate quite high blown slurs.")
     (property-description 'y-free number? "minimal vertical gap between slur and noteheads or stems")
     (property-description 'control-points list? "[internal] control points of bezier curve")
     (property-description 'extremity-rules  list? "an alist (procedure slur dir) -> attachment to determine the attachment (see above).  If procedure returns #t, attachment is used.  Otherwise, the next procedure is tried.")
     (property-description 'extremity-offset-alist list? "an alist (attachment stem-dir*dir slur-dir*dir) -> offset.  The offset adds to the centre of the notehead, or stem.")
     (property-description 'thickness list? "The thickness[stafflinethickness] of slur in the centre.")
     (property-description 'dashed number? "[FIXME: use dash-period/dash length; see text-spanner] number representing the length of the dashes.")

    )
   )
  )

(define side-position-interface
  (lily-interface
   'side-position-interface
   "Position a victim object (this one) next to other objects (the support)."
   (list
   (property-description 'side-support list? "the support, a list of score elements")
   (property-description 'direction-source ly-element? "in case side-relative-direction is set, which element  to get the direction from ")
    (property-description 'direction dir? "where to put the victim object (left or right?)")
    (property-description 'side-relative-direction dir? "if set: get the direction from a different object, and multiply by this.")
    (property-description 'minimum-space number? "minimum distance that the victim should move (after padding)")
    (property-description 'padding number? "add this much extra space between victim and support")
    (property-description 'self-alignment-X number? "real number: -1 = left aligned, 0 = center, 1 right-aligned in X direction. Set to an element pointer, if you want that element to be the center. ")
    (property-description 'self-alignment-Y number? "like self-alignment-X but for Y axis")
    
    )
  ))

(define accidentals-interface
  (lily-interface
   'accidentals-interface
   "Accidentals"
   (list
    (property-description 'left-padding number? "space left of accs")
    (property-description 'right-padding number? "space right of accs")     
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
    (property-description 'between-system-string string? "string
 to dump between two systems. Useful for forcing pagebreaks")
    (property-description 'spacing-procedure procedure? "procedure taking
graphical element as argument. This is called after before-line-breaking-callback, but before the actual line breaking itself.  Return value is ignored")
    (property-description 'before-line-breaking-callback procedure?
			  "Procedure taking graphical element as argument.
This procedure is called (using dependency resolution) before line breaking, but after generating discretionary items. Return value is ignored")
    (property-description 'after-line-breaking-callback procedure?
			  "Procedure taking graphical element as argument.
This procedure is called (using dependency resolution) after line breaking. Return value is ignored")
    (property-description 'all-elements list? "list of all score elements in this line. Needed for protecting elements from GC.")
    (property-description 'columns list? "list of all paper columns")
    )))

(define note-head-interface
  (lily-interface
   'note-head-interface
   "Note head"
   (list
    (property-description 'style symbol? "symbol that sets note head style")
    )
   ))

(define note-name-interface
  (lily-interface
   'note-name-interface
   "Note name"
   (list
    (property-description 'style symbol? "symbol that sets note name style")
    )
   ))


(define rhythmic-head-interface
  (lily-interface
   'rhythmic-head-interface
   "Note head or rest"
   (list
    (property-description 'dot ly-element? "reference to Dots object.")
    (property-description 'stem ly-element? "pointer to Stem object")
    (property-description 'duration-log integer? "2-log of the notehead duration")
    )))

(define rest-interface
  (lily-interface
   'rest-interface
   "a rest"
   (list
    (property-description 'style string? "string specifying glyph style"))))

(define tuplet-bracket-interface
  (lily-interface
   'tuplet-bracket-interface
   "A bracket with a number in the middle, used for tuplets." 
   (list
    (property-description 'beams list? "list of beam ptrs.")
    (property-description 'columns list? " list of note-columns.")
    (property-description 'number-gap number? "")
    (property-description 'delta-y number? "amount of ascension")
    (property-description 'thick number? "thickness, in stafflinethickness")
    )
))


(define align-interface
  (lily-interface
   'align-interface
   " Order elements top to bottom/left to right/right to left etc."
   (list
    (property-description 'stacking-dir  dir? "stack contents of elements in which direction ?")
    (property-description 'align-dir  dir? "Which side to align? -1: left side, 0: centered around center-element if not nil, or around center of width), 1: right side")
    (property-description 'threshold  number-pair? "(cons MIN MAX), where MIN and MAX are dimensions in staffspace")
    (property-description 'alignment-done  boolean? "boolean to administrate whether we've done the alignment already (to ensure that the process is done only once)")
    (property-description 'center-element ly-element? "element which will be at the
center of the group after aligning (when using
Align_interface::center_on_element). The center element should have
this object as a reference point.")
    (property-description 'elements  list? "to be aligned elements ")
    (property-description 'axes  list? "list of axis numbers. Should contain only one number.")
    )))    

(define aligned-interface
  (lily-interface
   'aligned-interface
   "read by align-interface"
   (list
    (property-description 'minimum-space number-pair? "(cons LEFT RIGHT)")
    (property-description 'extra-space number-pair? "(cons LEFT RIGHT)")
    )))

(define break-aligned-interface
  (lily-interface
   'break-aligned-interface
   "Items that are aligned in prefatory matter"
   (list
    (property-description 'break-align-symbol symbol? "the index in the spacing table (symbol) of the to be aligned item.")
    (property-description 'visibility-lambda procedure? "a function that takes the break direction and returns a  cons of booleans containing (TRANSPARENT . EMPTY)")
    (property-description 'breakable boolean? "boolean indicating if this is a breakable item (clef, barline, key sig, etc.)")
    )))

(define chord-name-interface
  (lily-interface
   'chord-name-interface
   "generate a chord name"
   (list
    (property-description 'pitches list? "list of musical-pitch")
    (property-description 'inversion list? " musical-pitch, optional")
    (property-description 'bass list? " musical-pitch, optional")
   )))

(define time-signature-interface
  (lily-interface
   'time-signature-interface
   "A time signature, in different styles"
   (list
    (property-description 'fraction number-pair? "")
    (property-description 'style string? "")
    )))

(define bar-line-interface
  (lily-interface
   'bar-line-interface
   "Bar line"
   (list
    (property-description 'barsize-procedure procedure? "how to compute the size of a bar line")
    (property-description 'kern number? "space after a thick line")
    (property-description 'thin-kern number? "space after a hair-line")
    (property-description 'hair-thickness number? "thickness, measured in stafflinethickness")
    (property-description 'thick-thickness number? "thickness, measured in stafflinethickness")
    (property-description 'glyph string? "what kind barline? A concatenation of |, : and .")
    (property-description 'bar-size number? "")
    (property-description 'break-glyph-function procedure? "function taking glyph and break-direction, returning the glyph at a line break")
   )))




(define hairpin-interface
  (lily-interface
   'hairpin-interface
   "hairpin crescendo"
   (list
    (property-description 'grow-direction dir? "crescendo or decrescendo?")
    (property-description 'thickness number? "thickness, measured in stafflinethickness")
    (property-description 'height number? "height, measured in staffspace in ")
    )))

(define arpeggio-interface
  (lily-interface
   'arpeggio-interface
   "arpeggio"
   (list
    (property-description 'stems list? "list of stem objects, corresponding to the notes that the arpeggio has to be before.")
    )
   )
  )

(define note-collision-interface
  (lily-interface
   'note-collision-interface
   "note collision"
   (list
    (property-description 'note-width 'number? "unit for horizontal translation, measured in staff-space.")
    )   )  )
(define dot-interface
  (lily-interface
   'dots-interface
   "The dots to go with a notehead/rest.  A separate interface, since they
  are a party in collision resolution."
   (list
    (property-description 'dot-count integer? "number of dots")
    )))

(define text-interface
  (lily-interface
   'text-interface
   "A scheme markup text"
   (list
    (property-description 'text (lambda (x) (or (string? x) (list? x))) "the scheme markup text.  Either a string, or a list of which the CAR is a markup '(MARKUP text text ...).  MARKUP is either a CONS: an element property '(key . value) or a symbol: an abbreviation for a list of element properties.  These abbreviations are currently defined: rows lines roman music bold italic named super sub text, as well as all font-style's.")
    (property-description 'font-style string? "font definition for a special purpose, one of: finger volta timesig mark script large Large dynamic")
    (property-description 'font-series string? "partial font definition: medium, bold")
    (property-description 'font-shape string?  "partial font definition: upright or italic")
    (property-description 'font-family string? "partial font definition: music roman braces dynamic math ...")
    (property-description 'font-name string? "partial font definition: base name of font file FIXME: should override other partials")
    (property-description 'font-point string? "partial font definition: exact font size in points FIXME: should override font-size")
    (property-description 'font-size string? "partial font definition: the relative size, 0 is style-sheet's normal size, -1 is smaller, +1 is bigger")
    (property-description 'align number? "the alignment of the text, 0 is horizontal, 1 is vertical")
    (property-description 'lookup symbol? "lookup method: 'value for plain text, 'name for character-name")
    (property-description 'raise number? "height for text to be raised (a negative value lowers the text")
    (property-description 'kern number? "amount of extra white space to add before text.  This is `relative'(?) to the current alignment.")
    (property-description 'magnify number? "the magnification factor.  FIXME: doesn't work for feta fonts")
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
    (property-description 'horizontal-space number? "amount of space to add after a note (in staff-space)")
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
    (property-description 'items-worth-living list? "list of interesting items. If empty in a particular system, clear that system.")


    )))

(define lyric-hyphen-interface
  (lily-interface
   'lyric-hyphen-interface
   "A centred hyphen is a simple line between lyrics used to divide
syllables.   The length of the hyphen line should stretch based on the
  size of the gap between syllables."
   (list
    
    (property-description 'thickness number? "thickness of line (in stafflinethickness)")
    (property-description 'height number? "vertical offset  (in staffspace)")

    (property-description 'minimum-length number? "try to make the hyphens at least this long. Also works as a scaling parameter for the length")
    (property-description 'word-space number? "elongate left by this much (FIXME: cumbersome semantics)")
    )))

(define key-signature-interface
  (lily-interface
   'key-signature-interface
   "A group of  accidentals."
   (list
    (property-description 'c0-position  integer? "integer indicating the position of central C")
    (property-description 'old-accidentals  list? "list of (pitch, accidental) pairs")
    (property-description 'new-accidentals  list? "list of (pitch, accidental) pairs")
    )))

(define lyric-extender-interface
  (lily-interface
   'lyric-extender-interface
   "The extender is a simple line at the baseline of the lyric
  that helps show the length of a melissima (tied/slurred note)."
   (list
    (property-description 'word-space  number? "")
    (property-description 'height  number? "in stafflinethickness")
    (property-description 'right-trim-amount  number? "")
    )))


(define lyric-syllable-interface
  (lily-interface
   'lyric-syllable-interface
   "a single piece of lyrics"
   (list
    (property-description 'word-space  number? "")
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
   "A rest that spans a whole number of measures."
   (list
    
    (property-description 'columns  list? "list of paper-columns")
    (property-description 'expand-limit  integer? "maximum number of measures expanded in church rests")
    (property-description 'minimum-width number? "minimum-width of rest symbol, in staffspace")
    (property-description 'padding  number? "padding between number and rest. Measured in staffspace.")
    )))

(define paper-column-interface
  (lily-interface
   'paper-column-interface
   ""
   (list
    (property-description 'when moment? "when does this column happen?")
    (property-description 'bounded-by-me list? "list of spanners that have this
column as start/begin point. Only columns that have elements or act as bounds are spaced.")
    (property-description 'dir-list  list? "list of stem directions")
    (property-description 'shortest-playing-duration  moment? "duration of the shortest playing in that column.")
    (property-description 'shortest-starter-duration  moment? "duration of the shortest notes that starts exactly in this column.")
    (property-description 'contains-grace  boolean? "Used to widen entries for grace notes.")
    (property-description 'extra-space  number-pair? "pair of distances")
    (property-description 'stretch-distance number-pair? "pair of distances")
    )))

(define spaceable-element-interface
  (lily-interface
   'spaceable-element-interface
   "An element (generally a Paper_column) that takes part in the
spacing problem. "
   (list
     (property-description 'minimum-distances list? "list of rods (ie. (OBJ . DIST) pairs)")
     (property-description 'ideal-distances  list? "(OBJ . (DIST . STRENGTH)) pairs")
     (property-description 'dir-list list? "list of stem directions, needed for optical spacing correction.")
     )))

(define rest-collision-interface
  (lily-interface
   'rest-collision-interface
   "Move around ordinary rests (not multi-measure-rests) to avoid
conflicts."
   (list
    (property-description 'maximum-rest-count integer? "kill off rests so we don't more than this number left.")
    (property-description 'minimum-distance number? "minimum distance between notes and rests.")
    (property-description 'elements list? "list of elements (NoteColumn,
generally) participating in the collision. The
<code>rest-collision</code> property in <code>elements</code> is set
to a pointer to the collision")
    )))

(define script-interface
  (lily-interface
   'script-interface
   ""
   (list
    (property-description 'script-priority number? "A sorting key that determines in what order a script is within a stack of scripts")
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
    (property-description 'maximum-duration-for-spacing moment? "space as if a duration of this type is available in this measure.")
    )))

(define staff-symbol-interface
  (lily-interface
   'staff-symbol-interface
   "This spanner draws the lines of a staff.  The middle line is
position 0."
   (list
    (property-description 'staff-space number? "Amount of line leading relative to global staffspace")
    (property-description 'line-count integer? "Number of staff lines")
    )))

(define stem-tremolo-interface
  (lily-interface
   'stem-tremolo-interface
   ""
   (list
    (property-description 'stem ly-element? "pointer to the stem object.")
    (property-description 'beam-width number? "width of the tremolo sign")
    (property-description 'beam-thickness number? "thickness, measured in staffspace")
    (property-description 'beam-space-function procedure? "function returning space given multiplicity")
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
    (property-description 'elements list? " -- list of items.")
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
    (property-description 'collapse-height number? "")
    (property-description 'thickness number? "thickness, measured in stafflinethickness")

    ; Should collapse into (bracket . ((height . ) ... ))
    ;
    (property-description 'arch-height number? "")
    (property-description 'arch-angle number? "")
    (property-description 'arch-thick number? "")
    (property-description 'arch-width number? "")
    (property-description 'bracket-thick number? "")
    (property-description 'bracket-width number? "")
    (property-description 'glyph symbol? "bar-line, bracket or brace")
    )))

(define text-spanner-interface
  (lily-interface
   'text-spanner-interface
   "generic text spanner"
   (list
    (property-description 'dash-period  number? "the length of one dash + white space")
    (property-description 'dash-length number? "the length of a dash")
    (property-description 'line-thickness number? "the thickness[stafflinethickness] of the line")
    (property-description 'edge-height pair? "a cons that specifies the heights of the vertical egdes '(LEFT-height . RIGHT-height)")
    (property-description 'edge-text pair? "a cons that specifies the texts to be set at the edges '(LEFT-text . RIGHT-text)")
    (property-description 'type string? "one of: line, dashed-line or dotted-line") ; SYMBOL!!?    
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
    (property-description 'staffline-clearance number? "don't get closer than this to stafflines.")
    (property-description 'control-points list? "List of 4 offsets (number-pairs) controlling the tie shape")
    (property-description 'heads pair? "pair of element pointers, pointing to the two heads of the  tie. ")
    (property-description 'details list? "alist of parameters for the curve shape")
    (property-description 'thickness number? "thickness, measured in stafflinethickness")
    (property-description 'x-gap number? "horizontal gap between notehead and tie")
    (property-description 'direction dir? "up or down?")    
    (property-description 'minimum-length number? "minimum length in staffspace")
    )))



(define tie-column-interface
  (lily-interface
   'tie-column-interface
   "that sets tie directions in a tied chord"
   (list
    )))

(define volta-bracket-interface
  (lily-interface
   'volta-bracket-interface
   "Volta bracket with number"
   (list
    (property-description 'bars  list? "list of barline ptrs.")
    (property-description 'thickness  number? "thickness, measured in stafflinethickness")
    (property-description 'height  number? "in staffspace ")
    )))

(define span-bar-interface
  (lily-interface
   'span-bar-interface
   ""
   (list
    )))

