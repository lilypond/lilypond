;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (music-map function music)
  "Apply @var{function} to @var{music} and all of the music it contains. "
  (let* ((es (ly:get-mus-property music 'elements))
         (e (ly:get-mus-property music 'element))
	 )

    (ly:set-mus-property! music 'elements 
	(map (lambda (y) (music-map  function y)) es))
	(if (ly:music? e)
	    (ly:set-mus-property! music 'element (music-map function  e)))
	(function music)
	))

(define-public (music-filter pred? music)
  "Filter out music expressions that do not satisfy PRED."
  
  (define (inner-music-filter pred? music)
    "Recursive function."
    (let* ((es (ly:get-mus-property music 'elements))
	   (e (ly:get-mus-property music 'element))
	   (as (ly:get-mus-property music 'articulations))
	   (filtered-as (filter ly:music? (map (lambda (y) (inner-music-filter pred? y)) as)))
	   (filtered-e (if (ly:music? e)
			   (inner-music-filter pred? e)
			   e))
	   (filtered-es (filter ly:music? (map (lambda (y) (inner-music-filter pred? y)) es)))
	   )

      (ly:set-mus-property! music 'element filtered-e)
      (ly:set-mus-property! music 'elements filtered-es)
      (ly:set-mus-property! music 'articulations filtered-as)

      ;; if filtering emptied the expression, we remove it completely.
      (if (or (pred? music)
	      (and (eq? filtered-es '()) (not (ly:music? e))
		   (or (not (eq? es '()))
		       (ly:music? e))))
	  (set! music '()))
      
      music))

  (set! music (inner-music-filter pred? music))
  (if (ly:music? music)
      music
      (make-music-by-name 'Music)	;must return music.
      ))

(define-public (remove-tag tag)
  (lambda (mus)
    (music-filter
     (lambda (m)
       (let* ((tags (ly:get-mus-property m 'tags))
	      (res (memq tag tags)))
       res)) mus)))

(define-public (display-music music)
  "Display music, not done with music-map for clarity of presentation."
  (display music)
  (display ": { ")
  
  (let* ((es (ly:get-mus-property music 'elements))
         (e (ly:get-mus-property music 'element))
	 )

    (display (ly:get-mutable-properties music))

    (if (pair?  es)
	(begin (display "\nElements: {\n")
	       (map display-music es)
	       (display "}\n")
	))
    
    
    (if (ly:music? e)
	(begin
	  (display "\nChild:")
	  (display-music e)
	  )
	)
    )
  (display " }\n")
  music
  )




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (shift-one-duration-log music shift dot)
  "  add SHIFT to ly:duration-log and optionally 
  a dot to any note encountered. This scales the music up by a factor 
  2^shift * (2 - (1/2)^dot)"

  (let*
      (
       (d (ly:get-mus-property music 'duration))
       )
    (if (ly:duration? d)
	(let* (
	       (cp (ly:duration-factor d))
	       (nd (ly:make-duration (+ shift (ly:duration-log d))
				     (+ dot (ly:duration-dot-count d))
				     (car cp)
				     (cdr cp)))
	       
	       )
	  (ly:set-mus-property! music 'duration nd)
	  ))
    music))



(define-public (shift-duration-log music shift dot)
  (music-map (lambda (x) (shift-one-duration-log x shift dot))
	     music))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clusters.

(define-public (note-to-cluster music)
  "Replace NoteEvents by ClusterNoteEvents."
  (if (eq? (ly:get-mus-property music 'name) 'NoteEvent)
      (let* ((cn (make-music-by-name 'ClusterNoteEvent)))

	     (ly:set-mus-property! cn 'pitch (ly:get-mus-property music 'pitch))
	     (ly:set-mus-property! cn 'duration (ly:get-mus-property music 'duration))
	     cn)
      music))

(define-public (notes-to-clusters music)
  (music-map note-to-cluster music))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; repeats.

(define-public (unfold-repeats music)
"
This function replaces all repeats  with unfold repeats. It was 
written by Rune Zedeler. "
  (let* ((es (ly:get-mus-property music 'elements))
         (e (ly:get-mus-property music 'element))
         (n  (ly:music-name music)))
 
    (if (equal? n "Repeated_music")
        (begin
	  (if (equal?
	       (ly:get-mus-property music 'iterator-ctor)
	       Chord_tremolo_iterator::constructor)
	      (shift-duration-log music  (ly:intlog2 (ly:get-mus-property music 'repeat-count)) 0)
	      )
          (ly:set-mus-property!
           music 'length Repeated_music::unfolded_music_length)
	  (ly:set-mus-property!
	   music 'start-moment-function Repeated_music::first_start)
          (ly:set-mus-property!
           music 'iterator-ctor Unfolded_repeat_iterator::constructor)))

    (if (pair? es)
        (ly:set-mus-property!
         music 'elements
         (map unfold-repeats es)))

    (if (ly:music? e)
        (ly:set-mus-property!
         music 'element
         (unfold-repeats e)))

    music))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; property setting music objs.

(define-public (make-grob-property-set grob gprop val)

  "Make a Music expression that sets GPROP to VAL in GROB. Does a pop first,
i.e.  this is not an override"
  
   (let* ((m (make-music-by-name  'OverrideProperty)))
     (ly:set-mus-property! m 'symbol grob)
     (ly:set-mus-property! m 'grob-property gprop)
     (ly:set-mus-property! m 'grob-value val)
     (ly:set-mus-property! m 'pop-first #t)
		
     m
   
   ))


(define-public (make-grob-property-revert grob gprop)
  "Revert the grob property GPROP for GROB."
   (let* ((m (make-music-by-name  'OverrideProperty)))
     (ly:set-mus-property! m 'symbol grob)
     (ly:set-mus-property! m 'grob-property gprop)
		
     m
   
   ))


(define-public (make-voice-props-set n)
  (make-sequential-music
   (append
      (map (lambda (x) (make-grob-property-set x 'direction
					       (if (odd? n) -1 1)))
	   '(Tie Slur Script TextScript Stem Dots))
      (list
       (make-grob-property-set 'NoteColumn 'horizontal-shift (quotient n 2))
       (make-grob-property-set 'MultiMeasureRest 'staff-position
			       (if (odd? n) -4 4)
			       )
       
       )
   )
  ))


(define-public (make-voice-props-revert)
  (make-sequential-music
   (list
      (make-grob-property-revert 'Tie 'direction)
      (make-grob-property-revert 'Dots 'direction)
      (make-grob-property-revert 'Stem 'direction)
      (make-grob-property-revert 'Slur 'direction)	    
      (make-grob-property-revert 'NoteColumn 'horizontal-shift)
   ))
  )


(define-public (context-spec-music m context . rest)
  "Add \\context CONTEXT = foo to M. "
  
  (let* ((cm (make-music-by-name 'ContextSpeccedMusic)))
    (ly:set-mus-property! cm 'element m)
    (ly:set-mus-property! cm 'context-type context)
    (if (and  (pair? rest) (string? (car rest)))
	(ly:set-mus-property! cm 'context-id (car rest))
    )
    cm
  ))

(define-public (make-apply-context func)
  (let*
      ((m (make-music-by-name 'ApplyContext)))

    (ly:set-mus-property! m 'procedure func)
    m
  ))

(define-public (make-sequential-music elts)
  (let*  ((m (make-music-by-name 'SequentialMusic)))
    (ly:set-mus-property! m 'elements elts)
    m
  ))

(define-public (make-simultaneous-music elts)
  (let*  ((m (make-music-by-name 'SimultaneousMusic)))
    (ly:set-mus-property! m 'elements elts)
    m
    ))

(define-public (make-event-chord elts)
  (let*  ((m (make-music-by-name 'EventChord)))
    (ly:set-mus-property! m 'elements elts)
    m
    ))


(define-public (make-nonevent-skip dur)
  (let*  ((m (make-music-by-name 'NonEventSkip)))
    (ly:set-mus-property! m 'duration dur)
    m
  ))

;;;;;;;;;;;;;;;;

;; mmrest
(define-public (make-multi-measure-rest duration location)
  (let*
      (
       (start (make-music-by-name 'MultiMeasureRestEvent))
       (stop  (make-music-by-name 'MultiMeasureRestEvent))
       (skip ( make-music-by-name 'SkipEvent))
       (ch (make-music-by-name 'BarCheck))
       (ch2  (make-music-by-name 'BarCheck))
       (seq  (make-music-by-name 'MultiMeasureRestMusicGroup))
       )

    (map (lambda (x) (ly:set-mus-property! x 'origin location))
	 (list start stop skip ch ch2 seq))
    (ly:set-mus-property! start 'span-direction START)
    (ly:set-mus-property! stop 'span-direction STOP)    
    (ly:set-mus-property! skip 'duration duration)
    (ly:set-mus-property! seq 'elements
     (list
      ch
      (make-event-chord (list start))
      (make-event-chord (list skip))
      (make-event-chord (list stop))
      ch2
      ))

    seq
    ))

(define-public (glue-mm-rest-texts music)
  "Check if we have R1*4-\\markup { .. }, and if applicable convert to
a property set for MultiMeasureRestNumber."
  
  (define (script-to-mmrest-text script-music)
    "Extract 'direction and 'text   from SCRIPT-MUSIC, and transform into property sets."
    
    (let*
	(
	 (text (ly:get-mus-property script-music 'text))
	 (dir (ly:get-mus-property script-music 'direction))
	 (p (make-music-by-name 'MultiMeasureTextEvent))
	 )

      (if (ly:dir? dir)
	  (ly:set-mus-property! p  'direction dir))
      (ly:set-mus-property! p 'text text)
      p
    ))
  
  (if (eq? (ly:get-mus-property music 'name)  'MultiMeasureRestMusicGroup)
      (let*
	  (
	   (text? (lambda (x) (memq 'script-event (ly:get-mus-property x 'types))))
	   (es (ly:get-mus-property  music 'elements))
	   (texts (map script-to-mmrest-text  (filter text? es)))
	   (others (remove text? es))
	   )
	(if (pair? texts)
	    (ly:set-mus-property!
	     music 'elements
	     (cons (make-event-chord texts) others)
	    ))
      ))
  music
  )


(define-public (make-property-set sym val)
  (let*
      (
       (m (make-music-by-name 'PropertySet))
       )
    (ly:set-mus-property! m 'symbol sym)
    (ly:set-mus-property! m 'value val)
    m
  ))



(define-public (make-ottava-set octavation)
  (let*
      ((m (make-music-by-name 'ApplyContext)))
    
  
  (define (ottava-modify context)
    "Either reset centralCPosition to the stored original,
or remember old centralCPosition, add OCTAVATION to centralCPosition,
and set OTTAVATION to `8va', or whatever appropriate."
    (if (number? (ly:get-context-property  context 'centralCPosition))
	
	(if (= octavation 0)
	    (let*
		((where (ly:context-property-where-defined context 'centralCPosition))
		 (oc0 (ly:get-context-property context 'originalCentralCPosition)))

	      (ly:set-context-property! context 'centralCPosition oc0)
	      (ly:unset-context-property where 'originalCentralCPosition)
	      (ly:unset-context-property where 'ottavation))

	    (let*
		((where (ly:context-property-where-defined context 'centralCPosition))
		 (c0 (ly:get-context-property context 'centralCPosition))
		 (new-c0 (+ c0 (* -7 octavation)))
		 (string (cdr
			  (assoc octavation '((2 . "15ma")
					      (1 . "8va")
					      (0 . #f)
					      (-1 . "8va bassa")
					      (-2 . "15ma bassa"))))))

	      (ly:set-context-property! context 'centralCPosition new-c0)
	      (ly:set-context-property! context 'originalCentralCPosition c0)
	      (ly:set-context-property! context 'ottavation string)
	      
	      ))))

  (ly:set-mus-property! m 'procedure  ottava-modify)
  (context-spec-music m 'Staff)
  ))

(define-public (set-octavation ottavation)
  (ly:export (make-ottava-set ottavation)))

(define-public (make-time-signature-set num den . rest)
  " Set properties for time signature NUM/DEN.
Rest can contain a list of beat groupings 

"
  
  (let*
      (
       (set1 (make-property-set 'timeSignatureFraction (cons num den) ))
       (beat (ly:make-moment 1 den))
       (len  (ly:make-moment num den))
       (set2 (make-property-set 'beatLength beat))
       (set3 (make-property-set 'measureLength len))
       (set4 (make-property-set 'beatGrouping (if (pair? rest)
						  (car rest)
						  '())))
       (basic  (list set1 set2 set3 set4)))

    (context-spec-music
     (make-sequential-music basic) 'Timing)))

(define-public (set-time-signature num den . rest)
  (ly:export (apply make-time-signature-set `(,num ,den . ,rest))))

(define-public (make-penalty-music pen)
 (let
     ((m (make-music-by-name 'BreakEvent)))
   (ly:set-mus-property! m 'penalty pen)
   m))

(define-public (make-articulation name)
  (let* (
	 (m (make-music-by-name 'ArticulationEvent))
      )
      (ly:set-mus-property! m 'articulation-type name)
      m
  ))

(define-public (make-lyric-event string duration)
  (let* ((m (make-music-by-name 'LyricEvent)))

    (ly:set-mus-property! m 'duration duration)
    (ly:set-mus-property! m 'text string)
    m))

(define-public (make-span-event type spandir)
  (let* (
	 (m (make-music-by-name  type))
	 )
    (ly:set-mus-property! m 'span-direction spandir)
    m
    ))

(define-public (set-mus-properties! m alist)
  "Set all of ALIST as properties of M." 
  (if (pair? alist)
      (begin
	(ly:set-mus-property! m (caar alist) (cdar alist))
	(set-mus-properties! m (cdr alist)))
  ))



(define-public (music-separator? m)
  "Is M a separator?"
  (let* ((ts (ly:get-mus-property m 'types )))
    (memq 'separator ts)
  ))


;;; splitting chords into voices.

(define (voicify-list lst number)
   "Make a list of Musics.

   voicify-list :: [ [Music ] ] -> number -> [Music]
   LST is a list music-lists.
"

   (if (null? lst) '()
       (cons (context-spec-music
	      (make-sequential-music
	       (list
		(make-voice-props-set number)
		(make-simultaneous-music (car lst))))

	      'Voice  (number->string number))
	      (voicify-list (cdr lst) (+ number 1))
       ))
   )

(define (voicify-chord ch)
  "Split the parts of a chord into different Voices using separator"
   (let* ((es (ly:get-mus-property ch 'elements)))
     
     (ly:set-mus-property!  ch 'elements
       (voicify-list (split-list es music-separator?) 0))
     ch
   ))

(define (voicify-music m)
   "Recursively split chords that are separated with \\ "
   
   (if (not (ly:music? m))
       (begin (display m)
       (error "not music!"))
       )
   (let*
       ((es (ly:get-mus-property m 'elements))
	(e (ly:get-mus-property m 'element))
	)
     (if (pair? es)
	 (ly:set-mus-property! m 'elements (map voicify-music es)))
     (if (ly:music? e)
	 (ly:set-mus-property! m 'element  (voicify-music e)))
     (if
      (and (equal? (ly:music-name m) "Simultaneous_music")
	   (reduce (lambda (x y ) (or x y)) #f (map music-separator? es)))
      (voicify-chord m)
      )

     m
     ))

(define-public (empty-music)
  (ly:export (make-music-by-name 'Music))
  )
;;;

; Make a function that checks score element for being of a specific type. 
(define-public (make-type-checker symbol)
  (lambda (elt)
    ;;(display  symbol)
    ;;(eq? #t (ly:get-grob-property elt symbol))
    (not (eq? #f (memq symbol (ly:get-grob-property elt 'interfaces))))))

(define-public ((outputproperty-compatibility func sym val) grob g-context ao-context)
  (if (func grob)
      (ly:set-grob-property! grob sym val)))


(define-public ((set-output-property grob-name symbol val)  grob grob-c context)
   "Usage:

\\applyoutput #(set-output-property 'Clef 'extra-offset '(0 . 1))

"
   
   (let*
       ((meta (ly:get-grob-property grob 'meta)))

     (if (equal?  (cdr (assoc 'name meta)) grob-name)
	 (ly:set-grob-property! grob symbol val)
	 )))


;;
(define-public (smart-bar-check n)
  "Make  a bar check that checks for a specific bar number. 
"
  (let*
      (
       (m (make-music-by-name 'ApplyContext))
       )
    
    (define (checker tr)
      (let* ((bn (ly:get-context-property tr 'currentBarNumber)))
	(if (= bn  n)
	    #t
	    (error
	     (format "Bar check failed, we should have reached ~a, instead at ~a\n"
		     n bn ))
	    )))

    (ly:set-mus-property! m 'procedure checker)
    m
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; warn for bare chords at start.

(define (has-request-chord elts)
  (reduce (lambda (x y) (or x y)) #f (map (lambda (x) (equal? (ly:music-name x)
							   "Request_chord")) elts)
  ))

(define (ly:music-message music msg)
  (let*
      (
      (ip (ly:get-mus-property music 'origin))
      )

    (if (ly:input-location? ip)
	(ly:input-message ip msg)
	(ly:warn msg))
  ))
  
(define (check-start-chords music)
  "Check music expression for a Simultaneous_music containing notes\n(ie. Request_chords), without context specification. Called  from parser."
  
     (let*
       ((es (ly:get-mus-property music 'elements))
	(e (ly:get-mus-property music 'element))
	(name (ly:music-name music)) 
	)

       (cond 
	 ((equal? name "Context_specced_music") #t)
	 ((equal? name "Simultaneous_music")

	  (if (has-request-chord es)
	      (ly:music-message music "Starting score with a chord.\nPlease insert an explicit \\context before chord")
	      (map check-start-chords es)))
	 
	 ((equal? name "Sequential_music")
	   (if (pair? es)
	       (check-start-chords (car es))))
	  (else (if (ly:music? e) (check-start-chords e )))
       
       ))
     music
     )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; setting stuff for grace context.
;;

(define (vector-extend v x)
  "Make a new vector consisting of V, with X added to the end."
  (let*
      ((n (vector-length v))
       (nv (make-vector (+ n 1) '())))

    
    (vector-move-left! v 0 n nv 0)
    (vector-set! nv n x)
    nv))


(define (vector-map f v)
  "Map  F over V. This function returns nothing."
  (do
      ((n (vector-length v))
       (i 0 (+ i 1)))
      ((>= i n))
  
    (f (vector-ref v i))))

(define (vector-reverse-map f v)
  "Map  F over V, N to 0 order. This function returns nothing."
  (do
      ((i (- (vector-length v) 1) (- i 1)))
      ((< i 0))
  
    (f (vector-ref v i))))

;; TODO:  make a remove-grace-property too.
(define-public (add-grace-property context-name grob sym val)
  "Set SYM=VAL for GROB in CONTEXT-NAME. "
  (define (set-prop context)
    (let*
	((where (ly:context-property-where-defined context 'graceSettings))
	 (current (ly:get-context-property where 'graceSettings))
	 (new-settings (vector-extend current (list context-name grob sym val)))
	 )
      (ly:set-context-property! where 'graceSettings new-settings)))
    
    (ly:export (context-spec-music (make-apply-context set-prop) 'Voice)))


(define-public (set-start-grace-properties context)
  (define (execute-1 x)
    (let*
	((tr (ly:translator-find context (car x))))

      (if (ly:context? tr)
	  (ly:context-pushpop-property tr (cadr x) (caddr x) (cadddr x))
	  )))
  
  (let*
      ((props (ly:get-context-property context 'graceSettings)))
    (if (vector? props)
	(vector-map execute-1 props))))

(define-public (set-stop-grace-properties context)
  (define (execute-1 x)
    (let*
	((tr (ly:translator-find context (car x))))
      (if (ly:context? tr)
	  (ly:context-pushpop-property tr (cadr x) (caddr x))
	  )))
  
  (let*
      ((props (ly:get-context-property context 'graceSettings)))
    (if (vector? props)
	(vector-reverse-map execute-1 props))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; switch it on here, so parsing and init isn't checked (too slow!)
;;
;; automatic music transformations.

(define (switch-on-debugging m)
  (set-debug-cell-accesses! 15000)
  m)

(define-public toplevel-music-functions
  (list check-start-chords
	voicify-music
	(lambda (x) (music-map glue-mm-rest-texts x))
; switch-on-debugging
	))


