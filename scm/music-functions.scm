
(define (denominator-tuplet-formatter mus)
  (number->string (ly-get-mus-property mus 'denominator)))

(define (fraction-tuplet-formatter mus)
  (string-append (number->string (ly-get-mus-property mus 'numerator))
		 ":"
		 (number->string (ly-get-mus-property mus 'denominator))
		 ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (shift-duration-log music shift )
  "Recurse through music, adding SHIFT to duration-log to any note
encountered. This scales the music up by a factor 2^k."
  (let* ((es (ly-get-mus-property music 'elements))
         (e (ly-get-mus-property music 'element))
         (n  (ly-music-name music))
	 (f  (lambda (x)  (shift-duration-log x shift)))
	 )
    (if (or (equal? n "Note_req")
	    (equal? n "Rest_req"))
	(let* (
	      (d (ly-get-mus-property music 'duration))
	      (cp (duration-factor d))
	      (nd (make-duration (+ shift (duration-log d))
				 (duration-dot-count d)
				 (car cp)
				 (cdr cp)))
	  
	      )
	  (ly-set-mus-property music 'duration nd)
	))

    (if (pair? es)
        (ly-set-mus-property
         music 'elements
         (map f es)))

    (if (music? e)
        (ly-set-mus-property
         music 'element
         (f e)))

    music))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (unfold-repeats music)
"
This function replaces all repeats  with unfold repeats. It was 
written by Rune Zedeler. "
  (let* ((es (ly-get-mus-property music 'elements))
         (e (ly-get-mus-property music 'element))
         (n  (ly-music-name music)))

    (if (equal? n "Repeated_music")
        (begin
	  (if (equal? (ly-get-mus-property 'type music) 'tremolo)
	      (shift-duration-log music (- (intlog2 (ly-get-mus-property 'repeat-count music))))
	      )
          (ly-set-mus-property
           music 'length Repeated_music::unfolded_music_length)
	  (ly-set-mus-property
	   music 'start-moment-function Repeated_music::first_start)
          (ly-set-mus-property
           music 'iterator-ctor Unfolded_repeat_iterator::constructor)))

    (if (pair? es)
        (ly-set-mus-property
         music 'elements
         (map unfold-repeats es)))

    (if (music? e)
        (ly-set-mus-property
         music 'element
         (unfold-repeats e)))

    music))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define  (pitchify-scripts music)
  "Copy the pitch fields of the Note_requests into  Text_script_requests, to aid
Fingering_engraver."
  (define (find-note musics)
    (filter-list (lambda (m) (equal? (ly-music-name m) "Note_req")) musics)
    )
  (define (find-scripts musics)
    (filter-list (lambda (m) (equal? (ly-music-name m) "Text_script_req")) musics))

  (let* (
	 (e (ly-get-mus-property music 'element))
	 (es (ly-get-mus-property music 'elements))
	 (notes (find-note es))
	 (pitch (if (pair? notes) (ly-get-mus-property (car  notes) 'pitch) #f))
	 )

    (if pitch
	(map (lambda (x) (ly-set-mus-property x 'pitch pitch)) (find-scripts es))
	)
	
    (if (pair? es)
        (ly-set-mus-property
         music 'elements
         (map pitchify-scripts es)))

    (if (music? e)
        (ly-set-mus-property
         music 'element
         (pitchify-scripts e)))

    music))


;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;


(define (make-grob-property-set grob gprop val)
  "Make a M-exp that sets GPROP to VAL in GROBS. Does a pop first, i.e.
this is not an override 
"
  
   (let* ((m (ly-make-music  "Music")))
     (ly-set-mus-property m 'iterator-ctor Push_property_iterator::constructor)
     (ly-set-mus-property m 'symbol grob)
     (ly-set-mus-property m 'grob-property gprop)
     (ly-set-mus-property m 'grob-value val)
     (ly-set-mus-property m 'pop-first #t)
		
     m
   
   ))
   
(define (make-grob-property-revert grob gprop)
  "Revert the grob property GPROP for GROB."
   (let* ((m (ly-make-music  "Music")))
     (ly-set-mus-property m 'iterator-ctor Pop_property_iterator::constructor)
     (ly-set-mus-property m 'symbol grob)
     (ly-set-mus-property m 'grob-property gprop)
		
     m
   
   ))
   
(define (make-voice-props-set n)
  (make-sequential-music
   (append
      (map (lambda (x) (make-grob-property-set x 'direction
					       (if (odd? n) -1 1)))
	   '(Tie Slur Stem Dots))
      (list (make-grob-property-set 'NoteColumn 'horizontal-shift (quotient n 2)))
   )
  ))

(define (make-voice-props-revert)
  (make-sequential-music
   (list
      (make-grob-property-revert 'Tie 'direction)
      (make-grob-property-revert 'Dots 'direction)
      (make-grob-property-revert 'Stem 'direction)
      (make-grob-property-revert 'Slur 'direction)	    
      (make-grob-property-revert 'NoteColumn 'horizontal-shift)
   ))
  )

(define (context-spec-music m context . rest)
  "Add \context CONTEXT = foo to M. "
  
  (let* ((cm (ly-make-music "Context_specced_music")))
    (ly-set-mus-property cm 'element m)
    (ly-set-mus-property cm 'context-type context)
    (if (and  (pair? rest) (string? (car rest)))
	(ly-set-mus-property cm 'context-id (car rest))
    )
    cm
  ))

(define (make-sequential-music elts)
  (let*  ((m (ly-make-music "Sequential_music")))
    (ly-set-mus-property m 'elements elts)
    m
  ))
(define (make-simultaneous-music elts)
  (let*  ((m (ly-make-music "Simultaneous_music")))
    (ly-set-mus-property m 'elements elts)
    m
    ))
(define (music-separator? m)
  "Is M a separator."
  (let* ((n (ly-get-mus-property m 'name )))
    (and (symbol? n) (equal? 'separator n))
  ))

(define (split-one sep?  l acc)
  "Split off the first parts before separator and return both parts.

"
  (if (null? l)
      (cons acc '())
      (if (sep? (car l))
	  (cons acc (cdr l))
	  (split-one sep? (cdr l) (cons (car l) acc))
	  )
      ))

(define (split-list l sep?)
  (if (null? l)
      '()
      (let* ((c (split-one sep? l '())))
	(cons (reverse! (car c) '()) (split-list (cdr c) sep?))
	)
      )
  )

;; test code
; (display (split-list '(a b c / d e f / g) (lambda (x) (equal? x '/))) )


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

	      "Voice"  (number->string number))
	      (voicify-list (cdr lst) (+ number 1))
       ))
   )

(define (voicify-chord ch)
  "Split the parts of a chord into different Voices using separator"
   (let* ((es (ly-get-mus-property ch 'elements)))


     (ly-set-mus-property  ch 'elements
       (voicify-list (split-list es music-separator?) 0))
     ch
   ))

(define (voicify-music m)
   "Recursively split chords that are separated with \\ "
   
   (if (not (music? m))
       (begin (display m)
       (error "not music!"))
       )
   (let*
       ((es (ly-get-mus-property m 'elements))
	(e (ly-get-mus-property m 'element))
	)
     (if (pair? es)
	 (ly-set-mus-property m 'elements (map voicify-music es)))
     (if (music? e)
	 (ly-set-mus-property m 'element  (voicify-music e)))
     (if
      (and (equal? (ly-music-name m) "Simultaneous_music")
	   (reduce (lambda (x y ) (or x y)) 	(map music-separator? es)))
      (voicify-chord m)
      )

     m
     ))

;;;

;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;

(define (has-request-chord elts)
  (reduce (lambda (x y) (or x y)) (map (lambda (x) (equal? (ly-music-name x)
							   "Request_chord")) elts)
  ))

(define (ly-music-message music msg)
  (let* (
      (ip (ly-get-mus-property music 'origin))
      )

    (if (ly-input-location? ip)
	(ly-input-message ip msg)
	(ly-warn msg))
  ))
  
(define (check-start-chords music)
  "Check music expression for a Simultaneous_music containing notes\n(ie. Request_chords), without context specification. Called  from parser."
  
     (let*
       ((es (ly-get-mus-property music 'elements))
	(e (ly-get-mus-property music 'element))
	(name (ly-music-name music)) 
	)

       (cond 
	 ((equal? name "Context_specced_music") #t)
	 ((equal? name "Simultaneous_music")

	  (if (has-request-chord es)
	      (ly-music-message music "Starting score with a chord.\nPlease insert an explicit \\context before chord")
	      (map check-start-chords es)))
	 
	 ((equal? name "Sequential_music")
	   (if (pair? es)
	       (check-start-chords (car es))))
	  (else (if (music? e) (check-start-chords e )))
       
       ))
     music
     )


(define toplevel-music-functions (list check-start-chords voicify-music))
