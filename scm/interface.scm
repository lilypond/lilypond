

; should include default value?


;;; FIXME: naming.
;;; Score elements are called `objects' now and then, which gets
;;; rather confusing, we now have `elements', `items', `spanners'
;;; and `objects'.


(define (lily-interface symbol description props)
  (list symbol
	description
	props
	)
  )


(define (element-description name . interfaces)
  (let* ((ifs (cons general-element-interface interfaces))
	 (props (map caddr ifs))
;	 (prop-typep-pairs (map (lambda (x) (cons (car x) (cadr x)))
;					(apply append props)))
	 (syms (map car ifs))
	)
    (list (cons 'separator "\n\n\n")	;easy printing.
	  (cons 'name name)
	  (cons 'interfaces syms)
	  (cons 'interface-descriptions ifs)
	  ; (cons 'interface-descriptions (cadr merged))
	  ;; description of the element itself?
;	  (cons 'properties prop-typep-pairs)
  )))

(define general-element-interface
  (lily-interface
   'general-element-interface
   "All elements support this"
   '(
    X-offset-callbacks 
    Y-offset-callbacks 
    X-extent-callback 
    Y-extent-callback 
    font-relative-size 
    extra-offset 
    interfaces  
    dependencies 
    no-spacing-rods 
    extra-extent-X 
    extra-extent-Y 
    minimum-extent-X 
    minimum-extent-Y 
    origin 
    transparent 
    ))
  )

(define beam-interface
  (lily-interface
   'beam-interface
   "A beam.

#'thickness= weight of beams, in staffspace
  "
   '(
    y-position 
    height 
    flag-width-function 
    damping 
    default-neutral-direction 
    thickness 
    space-function 
    beamed-stem-shorten 
    height-quants 
    vertical-position-quant-function 
    damping 
    outer-stem-length-limit 
    slope-limit 
    )
   ))

(define clef-interface
  (lily-interface
   'clef-interface
   "A clef sign"
   '(
    non-default 
    full-size-change 
    glyph 
    ))
  )

(define axis-group-interface
  (lily-interface
   'axis-group-interface
   "a group of coupled elements"
   '(
    axes 
   )))

(define note-column-interface
  (lily-interface
   'note-column-interface
   "Stem and noteheads combined"
   '(
    horizontal-shift 
    force-hshift 
    ))
  )

(define stem-interface
  (lily-interface
   'stem-interface
   "A stem"
   '(
    thickness 
    beamed-lengths 
    beamed-minimum-lengths 
    stem-centered 
    lengths 
    beam 
    stem-shorten 
    duration-log 
    beaming 
    default-neutral-direction 
    stem-end-position 
    support-head 
    heads 
    direction 
    stem-length 
    style 
    flag-style 
    dir-forced 
    )))


(define slur-interface
  (lily-interface
   'slur-interface
   "A slur"
   '(
    de-uglify-parameters 
    details 
    attachment 
    direction 
   attachment-offset 
     beautiful 
     y-free 
     control-points 
     extremity-rules  
     extremity-offset-alist 
     thickness 
     dashed 

    )
   )
  )

(define side-position-interface
  (lily-interface
   'side-position-interface
   "Position a victim object (this one) next to other objects (the support).
#'direction = where to put the victim object (left or right?)
"
   '(
   side-support 
   direction-source 
    direction 
    side-relative-direction 
    minimum-space 
    padding 
    self-alignment-X 
    self-alignment-Y 
    
    )
  ))

(define accidentals-interface
  (lily-interface
   'accidentals-interface
   "Accidentals"
   '(
    left-padding 
    right-padding 
    )
   ))

(define line-of-score-interface
  (lily-interface
   'line-of-score-interface
   "Super element, parent of all:

The columns of a score that form one line.  The toplevel element.  Any
element has a Line_of_score as both X and Y reference point. The
Paper_score contains one element of this type. Control enters the
Grob dependency calculation from this single Line_of_score
object."
   '(
    between-system-string 
    spacing-procedure 
    before-line-breaking-callback
    after-line-breaking-callback 
    all-elements 
    columns 
    )))

(define note-head-interface
  (lily-interface
   'note-head-interface
   "Note head"
   '(
    style 
    )
   ))

(define note-name-interface
  (lily-interface
   'note-name-interface
   "Note name"
   '(
    style 
    )
   ))


(define rhythmic-head-interface
  (lily-interface
   'rhythmic-head-interface
   "Note head or rest"
   '(
    dot 
    stem 
    duration-log 
    )))

(define rest-interface
  (lily-interface
   'rest-interface
   "a rest"
   '(style )))

(define tuplet-bracket-interface
  (lily-interface
   'tuplet-bracket-interface
   "A bracket with a number in the middle, used for tuplets." 
   '(
    beams 
    columns 
    number-gap 
    delta-y 
    tuplet-bracket-visibility 
    tuplet-number-visibility 
    parallel-beam 
    thick 
    )
))


(define align-interface
  (lily-interface
   'align-interface
   " Order elements top to bottom/left to right/right to left etc."
   '(
    stacking-dir  
    align-dir  
    threshold  
    alignment-done  
    center-element 
    elements  
    axes  
    )))    

(define aligned-interface
  (lily-interface
   'aligned-interface
   "read by align-interface"
   '(
    minimum-space 
    extra-space 
    )))

(define break-aligned-interface
  (lily-interface
   'break-aligned-interface
   "Items that are aligned in prefatory matter"
   '(
    break-align-symbol 
    visibility-lambda 
    breakable 
    )))

(define chord-name-interface
  (lily-interface
   'chord-name-interface
   "generate a chord name"
   '( pitches inversion bass)))

(define time-signature-interface
  (lily-interface
   'time-signature-interface
   "A time signature, in different styles"
   '( fraction style )))

(define bar-line-interface
  (lily-interface
   'bar-line-interface
   "Bar line.

This is a request to print a special bar symbol. It replaces the 
regular bar symbol with a special
symbol.  The argument @var{bartype} is a string which specifies the
kind of bar to print.  Options are @code{\":|\"}
@cindex \"|A@@@code{:|}
,
@code{\"|:\"}
@cindex \"|B@@@code{|:}
, @code{\":|:\"}
@cindex \"|C@@@code{:|:}
,
@code{\"||\"}
@cindex \"|D@@@code{||}
, @code{\"|.\"}
@cindex \"|E@@@code{|.}
,
@code{\".|\"}
@cindex \"|F@@@code{.|}
, and @code{\".|.\"}
@cindex \"|G@@@code{.|.}
. 

These produce, respectively, a right repeat, a left repeat, a double
repeat, a double bar, a start bar, an end bar, and a thick double bar.
If @var{bartype} is set to @code{\"empty\"} then nothing is printed,
but a line break is allowed at that spot.

"
   '( barsize-procedure kern thin-kern hair-thickness thick-thickness glyph bar-size break-glyph-function )))

(define hairpin-interface
  (lily-interface
   'hairpin-interface
   "hairpin crescendo.

padding -- horizontal padding. This is useful if a crescendo is set next to a text like `mf'

"
   '( grow-direction thickness height padding )
   ))

(define arpeggio-interface
  (lily-interface
   'arpeggio-interface
   "Functions and settings for drawing an arpeggio symbol (a wavy line left to noteheads."
   '(stems)
   )
  )

(define note-collision-interface
  (lily-interface
   'note-collision-interface
   "An object that handles collisions between notes with different
stem directions and horizontal shifts. Most of the interesting
properties are to be set in @ref{note-column-interface}"
   '(merge-differently-dotted note-width)
   ))


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
 
   '(style)
)
  )




(define dot-interface
  (lily-interface
   'dots-interface
   "The dots to go with a notehead/rest.  A separate interface, since they
  are a party in collision resolution.
 #'direction is the Direction to handle staff-line collisions in."
   '(direction dot-count)

   )) 

(define font-interface
  (lily-interface
   'font-interface
   "Any symbol that is typeset through fixed sets of glyphs (ie. fonts)"
   '(font-style font-series font-shape font-family font-name
font-point-size font-relative-size)
   ))


(define text-interface
  (lily-interface
   'text-interface
   "A scheme markup text"
   '(text align lookup raise kern magnify)))

(define dot-column-interface
  (lily-interface
   'dot-column-interface
   "Interface that groups dots so they form a column"
   '( )
   ))

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
   "Spanner that containing @code{separation-item-interface} elements to calculate rods"
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
   '(
    horizontal-space 
    )
   ))

(define hara-kiri-group-interface
  (lily-interface
   'hara-kiri-group-interface
   "  As Vertical_group_spanner, but keep track of interesting items.  If
  we don't contain any interesting items after linebreaking, then
  gracefully commit suicide.  Objective: don't disgrace Lily by
  typesetting empty lines in orchestral scores."
   '(
    items-worth-living 


    )))

(define line-spanner-interface
  (lily-interface
   'line-spanner-interface
   "Generic line drawn between two objects, eg. for use with glissandi.
gap is relative to the total length of the line.   "

   '(gap 
    dash-period 
    dash-length 
    line-thickness 
    type 
    )
   ))

(define lyric-hyphen-interface
  (lily-interface
   'lyric-hyphen-interface
   "A centred hyphen is a simple line between lyrics used to divide
syllables.   The length of the hyphen line should stretch based on the
  size of the gap between syllables."

   '( thickness height minimum-length word-space )
   ))

(define key-signature-interface
  (lily-interface
   'key-signature-interface
   "A group of  accidentals."
   '(
    c0-position  
    old-accidentals  
    new-accidentals  
    )))

(define lyric-extender-interface
  (lily-interface
   'lyric-extender-interface
   "The extender is a simple line at the baseline of the lyric
  that helps show the length of a melissima (tied/slurred note)."
   '(
    word-space  
    height  
    right-trim-amount  
    )))


(define lyric-syllable-interface
  (lily-interface
   'lyric-syllable-interface
   "a single piece of lyrics"
   '(
    word-space  
    )))


(define mark-interface
  (lily-interface
   'mark-interface
   "a rehearsal mark"
   '(
    )))

(define multi-measure-rest-interface
  (lily-interface
   'multi-measure-rest-interface
   "A rest that spans a whole number of measures.  For typesetting the
numbers, fields from font-interface may be used.

padding is the space between number and rest. Measured in staffspace.
 
"
   '(    columns expand-limit minimum-width padding )

   ))

(define paper-column-interface
  (lily-interface
   'paper-column-interface
   ""

   '(column-space-strength before-musical-spacing-factor
stem-spacing-correction before-grace-spacing-factor when bounded-by-me
dir-list shortest-playing-duration shortest-starter-duration
contains-grace extra-space stretch-distance ))

  )

(define spaceable-element-interface
  (lily-interface
   'spaceable-element-interface
   "An element (generally a Paper_column) that takes part in the
spacing problem. "
   '(
     minimum-distances 
     ideal-distances  
     dir-list 
     )))

(define rest-collision-interface
  (lily-interface
   'rest-collision-interface
   "Move around ordinary rests (not multi-measure-rests) to avoid
conflicts."
   '(
    maximum-rest-count 
    minimum-distance 
    elements 
    )))

(define script-interface
  (lily-interface
   'script-interface
   ""
   '(
    script-priority 
    )))

(define script-column-interface
  (lily-interface
   'script-column-interface
   "An interface that sorts scripts according to their @code{script-priority}"
   '( )))


(define spacing-spanner-interface
  (lily-interface
   'spacing-spanner-interface
   " SPACE = arithmetic_multiplier * ( C + log2 (TIME) ))
The space taken by a note is determined by the formula 

  

where TIME is the amount of time a note occupies.  The value of C is
chosen such that the smallest space within a measure is
arithmetic_basicspace:

  C = arithmetic_basicspace - log2 (mininum (SHORTEST, 1/8)) 

The smallest space is the one following the shortest note in the
measure, or the space following a hypothetical 1/8 note.  Typically
arithmetic_basicspace is set to a value so that the shortest note
takes about two noteheads of space (ie, is followed by a notehead of
space):

@example
   2*quartwidth = arithmetic_multiplier * ( C + log2 (SHORTEST) ))

   @{ using: C = arithmetic_basicspace - log2 (mininum (SHORTEST, 1/8)) @}
   @{ assuming: SHORTEST <= 1/8 @}

               = arithmetic_multiplier *
	       ( arithmetic_basicspace - log2 (SHORTEST) + log2 (SHORTEST) )

               = arithmetic_multiplier * arithmetic_basicspace

   @{ choose: arithmetic_multiplier = 1.0*quartwidth (why?) @}

               = quartwidth * arithmetic_basicspace

   =>	       

   arithmetic_basicspace = 2/1 = 2


If you want to space your music wider, use something like:

   arithmetic_basicspace = 4.;

@end example"
   '(
  maximum-duration-for-spacing 
    arithmetic-basicspace 
    arithmetic-multiplier 
    
    )))

(define staff-symbol-interface
  (lily-interface
   'staff-symbol-interface
   "This spanner draws the lines of a staff.  The middle line is
position 0."
   '(
    staff-space 
    line-count 
    )))

(define stem-tremolo-interface
  (lily-interface
   'stem-tremolo-interface
   ""
   '(
    stem 
    beam-width 
    beam-thickness 
    beam-space-function 
    )))

(define separation-item-interface
  (lily-interface
   'separation-item-interface
   "Item that computes widths to generate spacing rods.

Calc dimensions for the Separating_group_spanner; this has to be
   an item to get dependencies correct.  It can't be an element_group
   since these usually are in a different X_group
"
   '(
    elements 
     )))

(define sustain-pedal-interface
  (lily-interface
   'sustain-pedal-interface
   ""
   '(
    )))
(define system-start-delimiter-interface
  (lily-interface
   'system-start-delimiter-interface
   "#'style can be bar-line, bracket or brace"
   '(collapse-height thickness arch-height arch-angle arch-thick
		     arch-width bracket-thick bracket-width glyph )))

(define text-spanner-interface
  (lily-interface
   'text-spanner-interface
   "generic text spanner"
   '(
    dash-period  
    dash-length 
    line-thickness 
    edge-height 
    edge-text 
    type 
    )
))

(define tie-interface
  (lily-interface
   'tie-interface
   "A tie connecting two noteheads.
direction = Forced direction for all ties"
   
   '(
    staffline-clearance 
    control-points 
    heads 
    details 
    thickness 
    x-gap 
    direction 
    minimum-length 
    )))



(define tie-column-interface
  (lily-interface
   'tie-column-interface
   "that sets tie directions in a tied chord"
   '(direction)
   ))

(define volta-bracket-interface
  (lily-interface
   'volta-bracket-interface
   "Volta bracket with number"
   '(
    bars  
    thickness  
    height  
    )))

(define span-bar-interface
  (lily-interface
   'span-bar-interface
   ""
   '(
    )))




