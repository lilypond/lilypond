;;;; translation-functions.scm --
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c) 1998--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
;;;;		     Jan Nieuwenhuizen <janneke@gnu.org>

;; metronome marks
(define-public (format-metronome-markup dur count context)
  (let* ((note-mark (make-smaller-markup
		     (make-note-by-number-markup (ly:duration-log dur)
						 (ly:duration-dot-count dur)
						 1))))  
    (make-line-markup
     (list
      (make-general-align-markup Y DOWN note-mark)
      (make-simple-markup  "=")
      (make-simple-markup (number->string count))))))

(define-public (format-mark-alphabet mark context)
  (make-bold-markup (make-markalphabet-markup (1- mark))))

(define-public (format-mark-box-alphabet mark context)
  (make-bold-markup (make-box-markup (make-markalphabet-markup (1- mark)))))

(define-public (format-mark-circle-alphabet mark context)
  (make-bold-markup (make-circle-markup (make-markalphabet-markup (1- mark)))))

(define-public (format-mark-letters mark context)
  (make-bold-markup (make-markletter-markup (1- mark))))

(define-public (format-mark-numbers mark context)
  (make-bold-markup (number->string mark)))

(define-public (format-mark-barnumbers mark context)
  (make-bold-markup (number->string (ly:context-property context 'currentBarNumber))))

(define-public (format-mark-box-letters mark context)
  (make-bold-markup (make-box-markup (make-markletter-markup (1- mark)))))

(define-public (format-mark-circle-letters mark context)
  (make-bold-markup (make-circle-markup (make-markletter-markup (1- mark)))))

(define-public (format-mark-box-numbers mark context)
  (make-bold-markup (make-box-markup (number->string mark))))

(define-public (format-mark-circle-numbers mark context)
  (make-bold-markup (make-circle-markup (number->string mark))))

(define-public (format-mark-box-barnumbers mark context)
  (make-bold-markup (make-box-markup
    (number->string (ly:context-property context 'currentBarNumber)))))

(define-public (format-mark-circle-barnumbers mark context)
  (make-bold-markup (make-circle-markup
    (number->string (ly:context-property context 'currentBarNumber)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bass figures.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (format-bass-figure figure event context)
  (let* ((fig (ly:event-property event 'figure))
	 (fig-markup (if (number? figure)

			 ;; this is not very elegant, but center-aligning all digits
			 ;; is problematic with other markups, and shows problems
			 ;; in the (lack of) overshoot of feta alphabet glyphs.
			 
			 ((if (<= 10 figure)
			      (lambda (y) (make-translate-scaled-markup (cons -0.7 0) y))
			      identity)

			  (if (eq? #t (ly:event-property event 'diminished))
			      (markup #:slashed-digit figure)
			      (markup #:number (number->string figure 10))))
			 #f
			 ))
	 (alt (ly:event-property event 'alteration))
	 (alt-markup
	  (if (number? alt)
	      (markup
	       #:general-align Y DOWN #:fontsize
	       (if (not (= alt DOUBLE-SHARP))
		   -2 2)
	       (alteration->text-accidental-markup alt))
	      
	      #f))
	 (plus-markup (if (eq? #t (ly:event-property event 'augmented))
			  (markup #:number "+")
			  #f))

	 (alt-dir (ly:context-property context 'figuredBassAlterationDirection))
	 (plus-dir (ly:context-property context 'figuredBassPlusDirection))
	 )

    (if (and (not fig-markup) alt-markup)
	(begin
	  (set! fig-markup (markup #:left-align #:pad-around 0.3 alt-markup))
	  (set! alt-markup #f)))


    ;; hmm, how to get figures centered between note, and
    ;; lone accidentals too?
    
    ;;    (if (markup? fig-markup)
    ;;	(set!
    ;;	 fig-markup (markup #:translate (cons 1.0 0)
    ;;			    #:hcenter fig-markup)))

    (if alt-markup
	(set! fig-markup
	      (markup #:put-adjacent
		      fig-markup X
		      (if (number? alt-dir)
			  alt-dir
			  LEFT)
		      #:pad-x 0.2 alt-markup
		      )))

    
    (if plus-markup
	(set! fig-markup
	      (if fig-markup
		  (markup #:put-adjacent
			  fig-markup
			  X (if (number? plus-dir)
				plus-dir
				LEFT)
			  #:pad-x 0.2 plus-markup)
		  plus-markup)))
    
    (if (markup? fig-markup)
	(markup #:fontsize -2 fig-markup)
	empty-markup)

    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fret diagrams

(define-public (determine-frets context grob notes string-numbers)
  (define (ensure-number a b)
    (if (number? a)
	a
	b))
(let*
      ((tunings (ly:context-property context 'stringTunings))
       (minimum-fret (ensure-number
		      (ly:context-property context 'minimumFret) 0))
       (string-frets (determine-frets-mf notes string-numbers
					 minimum-fret
					 tunings)))

       	      
  (set! (ly:grob-property grob 'string-count) (length tunings))
  (set! (ly:grob-property grob 'string-frets) string-frets)

  ))

(define-public (determine-frets-mf notes string-numbers
				   minimum-fret
				   tunings)

  (define (calc-fret pitch string tuning)
    (- (ly:pitch-semitones pitch) (list-ref tuning (1- string))))

  (define (note-pitch a)
    (ly:event-property a 'pitch))

  (define (note-pitch<? a b)
    (ly:pitch<? (note-pitch a)
		(note-pitch b)))
  
  (define (note-ev-string ev)
    (let* ((articulations (ly:event-property ev 'articulations))
	   (string-found #f))

      (map (lambda (art)
	     (let*
		 ((num (ly:event-property art 'string-number)))

	       (if (number? num)
		   (set! string-found num))))
	   articulations)
      string-found))

  (let*
      ((free-strings (map 1+ (iota (length tunings))))
       (del-string (lambda (string)
		     (if (number? string)
			 (set! free-strings
			       (delete string free-strings)))))
       (string-qualifies (lambda (string pitch)
			   (and (>= (calc-fret pitch string tunings)
				    minimum-fret))))
       (string-frets '())
       (set-fret (lambda (note string)
		   (set! string-frets
			(acons string
			       (calc-fret (ly:event-property note 'pitch)
					  string tunings)
			       string-frets))
		   (del-string string)
		   ))
       

       )
    
    (for-each (lambda (note)
		(del-string (note-ev-string note)))
	      notes)


    (for-each
     (lambda (note)
       (if (note-ev-string note)
	   (set-fret note (note-ev-string note))
	   (let*
	       ((string (find (lambda (string) (string-qualifies string
								 (note-pitch note)))
			      (reverse free-strings))))

		(set-fret note string))))
     (sort notes note-pitch<?))

    
    (display string-frets)


    string-frets))
			  
