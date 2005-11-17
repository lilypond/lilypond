;;;; translation-functions.scm --
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c) 1998--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
;;;;		     Jan Nieuwenhuizen <janneke@gnu.org>

(define-public (denominator-tuplet-formatter mus)
  (number->string (ly:music-property mus 'denominator)))

(define-public (fraction-tuplet-formatter mus)
  (string-append
   (number->string (ly:music-property mus 'denominator))
   ":"
   (number->string (ly:music-property mus 'numerator))))

;; metronome marks
(define-public (format-metronome-markup event context)
  (let* ((dur (ly:music-property event 'tempo-unit))
       (count (ly:music-property event 'metronome-count))
       (note-mark (make-smaller-markup
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

(define-public (format-mark-letters mark context)
  (make-bold-markup (make-markletter-markup (1- mark))))

(define-public (format-mark-numbers mark context)
  (make-bold-markup (number->string mark)))

(define-public (format-mark-barnumbers mark context)
  (make-bold-markup (number->string (ly:context-property context 'currentBarNumber))))

(define-public (format-mark-box-letters mark context)
  (make-bold-markup (make-box-markup (make-markletter-markup (1- mark)))))

(define-public (format-mark-box-numbers mark context)
  (make-bold-markup (make-box-markup (number->string mark))))

(define-public (format-mark-box-barnumbers mark context)
  (make-bold-markup (make-box-markup
    (number->string (ly:context-property context 'currentBarNumber)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bass figures.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (format-bass-figure figure event context)
  (let* ((fig (ly:music-property event 'figure))
	 (fig-markup (if (number? figure)
			 (if (eq? #t (ly:music-property event 'diminished))
			     (markup #:slashed-digit figure)
			     (markup #:number (number->string figure 10)))
			 #f
			 ))
	 (alt (ly:music-property event 'alteration))
	 (alt-markup
	  (if (number? alt)
	      (markup
		      #:general-align Y DOWN #:smaller #:smaller
		      (alteration->text-accidental-markup alt))
	      
	      #f))
	 (plus-markup (if (eq? #t (ly:music-property event 'augmented))
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

