
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;

(define general-element-interface
  (lily-interface
   'general-element-interface
   "All elements support this"
   (list (property-description 'X-offset-callbacks list? "")
    (property-description 'Y-offset-callbacks list? "")
    (property-description 'X-extent-callback procedure? "")
    (property-description 'Y-extent-callback procedure? "")
    (property-description 'font-size integer? "")
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
    (property-description 'merge-differently-dotted boolean? "merge black noteheads with differing dot count.")
    ))
  )

(define stem-interface
  (lily-interface
   'stem-interface
   "A stem"
   (list
    (property-description 'thickness number? "")
    (property-description 'beamed-lengths list? "")
    (property-description 'beamed-minimum-lengths list? "")
    (property-description 'lengths list? "")
    (property-description 'stem-shorten list? "")
    (property-description 'default-neutral-direction dir? "")
    (property-description 'direction dir? "")
    (property-description 'stem-length number? "")
    (property-description 'style string? "") ; symbol!?
    (property-description 'flag-style string? "") ; symbol!?
    (property-description 'X-offset-callbacks list? "")
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
    (property-description 'y-free number? "? ")
    (property-description 'control-points list? "")
    (property-description 'extremity-rules  list? "")
    (property-description 'extremity-offset-alist list? "")
    (property-description 'thickness list? "")
    (property-description 'dash number? "number representing the length of the dashes.")
    )
   )
  )

(define side-position-interface
  (lily-interface
   'side-position-interface
   "put an element next to another one."
   (list
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
   "Note naem"
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
   "A bracket with a number in the middle" 
   (list
    (property-description 'beams list? "list of beam ptrs.")
    (property-description 'columns list? " list of note-columns.")
    (property-description 'number-gap number? "")
    (property-description 'delta-y number? "")
    (property-description 'thick number? "")
    )
))


(define align-interface
  (lily-interface
   'align-interface
   " Order elements top to bottom/left to right/right to left etc."
   (list
    (property-description 'stacking-dir  dir? "stack contents of elements in which direction ?")
    (property-description 'align-dir  dir? "Which side to align? -1: left side, 0: centered around center-element if not nil, or around center of width), 1: right side")
    (property-description 'threshold  pair? "(cons MIN MAX), where MIN and MAX are dimensions in staffspace")
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
    (property-description 'minimum-space pair? "(cons LEFT RIGHT)")
    (property-description 'extra-space pair? "(cons LEFT RIGHT)")
    )))

(define break-aligned-interface
  (lily-interface
   'break-aligned-interface
   "Items that are aligned in prefatory matter"
   (list
    (property-description 'break-align-symbol symbol? "the index in the spacing table (symbol) of the to be aligned item.")
    (property-description 'visibility-lambda procedure? "")
    (property-description 'breakable boolean? "")
    )))

(define chord-name-interface
  (lily-interface
   'chord-name-interface
   ""
   (list
    )))
(define time-signature-interface
  (lily-interface
   'time-signature-interface
   "A time signature, in different styles"
   (list
    (property-description 'fraction pair? "")
    (property-description 'style string? "")
    )))

(define bar-line-interface
  (lily-interface
   'bar-line-interface
   "Bar line"
   (list
    (property-description 'barsize-procedure procedure? "")
    (property-description 'kern number? "")
    (property-description 'thin-kern number? "")
    (property-description 'hair-thickness number? "")
    (property-description 'thick-thickness number? "")
    (property-description 'glyph string? "")
    (property-description 'bar-size number? "")
    (property-description 'break-glyph-function procedure? "")
   )))




(define text-spanner-interface
  (lily-interface
   'text-spanner-interface
   "generic text spanner"
   (list
    (property-description 'dash-period  number? "")
    (property-description 'dash-length number? "")
    (property-description 'line-thickness number? "")
    (property-description 'edge-height pair? "(leftheight . rightheight)")
    (property-description 'edge-text pair? "(lefttext . righttext)")
    (property-description 'text-style string? "") ; SYMBOL!!?
    (property-description 'type string? "line, dashed-line or dotted-line") ; SYMBOL!!?    
    )
))

(define hairpin-interface
  (lily-interface
   'hairpin-interface
   "hairpin crescendo"
   (list
    (property-description 'grow-direction dir? "")
    (property-description 'thickness number? "")
    (property-description 'height number? "")
    )))


(define arpeggio-interface
  (lily-interface
   'arpeggio-interface
   "arpeggio "
   (list
    (property-description 'stems list? "list of stem objects, corresponding to the notes that the  arp has to be before.")
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
   "A text"
   (list
    (property-description 'text string? "")
    (property-description 'style string? "")
    )))


(define dot-column-interface
  (lily-interface
   'dot-column-interface
   ""
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
   "Any kind of loudness sign"
   '()
    ))

(define separation-spanner-interface
  (lily-interface
   'separation-spanner-interface
   ""
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
   "seppuku"
   '()))

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
    (property-description 'c0-position  integer? "integer indicating the position of central C?")
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
   ""
   (list
    (property-description 'word-space  number? "")
    )))


(define mark-interface
  (lily-interface
   'mark-interface
   ""
   (list
    )))

(define multi-measure-rest-interface
  (lily-interface
   'multi-measure-rest-interface
   ""
   (list
    
    (property-description 'columns  list? "list of paper-columns")
    (property-description 'expand-limit  integer? "int : max number of measures expanded in church rests")
    (property-description 'minimum-width number? "Real in staffspace")
    (property-description 'padding  number? "staffspace")
    )))

(define paper-column-interface
  (lily-interface
   'paper-column-interface
   ""
   (list
    (property-description 'dir-list  list? "list of stem directions")
    (property-description 'shortest-playing-duration  moment? "duration of the shortest playing in that column.")
    (property-description 'shortest-starter-duration  moment? "duration of the shortest notes that starts exactly in this column.")
    (property-description 'contains-grace  boolean? "Used to widen entries for grace notes.")
    (property-description 'extra-space  pair? "pair of distances")
    (property-description 'stretch-distance pair? "pair of distances")
    )))

(define spaceable-element-interface
  (lily-interface
   'spaceable-element-interface
   ""
   (list
     (property-description 'minimum-distances list? "list of rods (ie. (OBJ . DIST) pairs)")
     (property-description 'ideal-distances  list? "(OBJ . (DIST . STRENGTH)) pairs")
     (property-description 'dir-list list? "list of stem directions.")
     )))

(define rest-collision-interface
  (lily-interface
   'rest-collision-interface
   ""
   (list
    (property-description 'maximum-rest-count integer? "")
    (property-description 'minimum-distance number? "")    
    )))

(define script-interface
  (lily-interface
   'script-interface
   ""
   (list
    (property-description 'script-priority number? "")
    )))

(define script-column-interface
  (lily-interface
   'script-column-interface
   ""
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
    (property-description 'staff-space number? "")
    (property-description 'line-count integer? "")
    )))

(define stem-tremolo-interface
  (lily-interface
   'stem-tremolo-interface
   ""
   (list
    (property-description 'stem ly-element? "pointer to the stem object.")
    (property-description 'beam-width number? "")
    (property-description 'beam-thickness number? "")
    (property-description 'beam-space-function procedure? "")
    )))

(define separation-item-interface
  (lily-interface
   'separation-item-interface
   ""
   (list
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
    (property-description 'thickness number? "")
    (property-description 'arch-height number? "")
    (property-description 'arch-angle number? "")
    (property-description 'arch-thick number? "")
    (property-description 'arch-width number? "")
    (property-description 'bracket-thick number? "")
    (property-description 'bracket-width number? "")
    )))

(define text-script-interface
  (lily-interface
   'text-script-interface
   ""
   (list
    
    )))

(define tie-interface
  (lily-interface
   'tie-interface
   ""
   (list
    (property-description 'staffline-clearance number? "")
    (property-description 'heads pair? "pair of element pointers, pointing to the two heads of the  tie. ")
    (property-description 'details list? "")
    (property-description 'thickness number? "")
    (property-description 'x-gap number? "")
    (property-description 'minimum-length number? "")
    )))



(define tie-column-interface
  (lily-interface
   'tie-column-interface
   ""
   (list
    )))

(define volta-bracket-interface
  (lily-interface
   'volta-bracket-interface
   "Volta bracket with number"
   (list
    (property-description 'bars  list? "list of barline ptrs.")
    (property-description 'thickness  number? "in stafflinethickness")
    (property-description 'height  number? "in staffspace ")
    )))

(define span-bar-interface
  (lily-interface
   'span-bar-interface
   ""
   (list
    )))

