;;;; double-plus-new-chord-name.scm -- Compile chord names
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c) 2003 Jan Nieuwenhuizen <janneke@gnu.org>

;;;; NOTE: this is experimental code
;;;; It only handles naming for steps 5 and up
;;;; There's no code for naming the base chord (steps 1-5)
;;;; or exceptions.


(define-module (scm double-plus-new-chord-name))
(debug-enable 'backtrace)
(use-modules (ice-9 regex)
	     (ice-9 string-fun)
	     (ice-9 format)
	     (guile)
	     (lily))

(define this-module (current-module))

(define (tail x)
  (car (reverse x)))

(define (list-minus a b)
  (if (pair? a)
      (if (pair? b)
	  (if (member (car a) b)
	      (list-minus (cdr a) b)
	      (cons (car a) (list-minus (cdr a) b)))
	  a)
      '()))

(define (assoc-default key alist default)
  (let ((value (assoc key alist)))
    (if value (cdr value) default)))
	 
(define (markup-join markups sep)
  "Return line-markup of MARKUPS, joining them with markup SEP"
  (if (pair? markups)
      (make-line-markup (reduce-list markups sep))
      empty-markup))

(define (ly:pitch-diff pitch tonic)
  (let ((simple-octave (- (ly:pitch-octave pitch) (ly:pitch-octave tonic)))
	(simple-notename
	 (- (ly:pitch-notename pitch) (ly:pitch-notename tonic))))
    (let ((octave (+ simple-octave (quotient simple-notename 7)
		     (if (< simple-notename 0) -1 0)))
	  (notename (modulo simple-notename 7)))
      (let ((alteration
	     (- (ly:pitch-semitones pitch)
		(ly:pitch-semitones tonic) 
		(ly:pitch-semitones (ly:make-pitch octave notename 0)))))
	(ly:make-pitch octave notename alteration)))))

(define (accidental->markup alteration)
  "Return accidental markup for ALTERATION."
  (if (= alteration 0)
      (make-line-markup (list empty-markup))
      (make-smaller-markup
       (make-musicglyph-markup
	(string-append "accidentals-" (number->string alteration))))))

(define (pitch->markup pitch)
  (make-line-markup
   (list
    (make-simple-markup
     (vector-ref #("C" "D" "E" "F" "G" "A" "B") (ly:pitch-notename pitch)))
    (make-normal-size-super-markup
     (accidental->markup (ly:pitch-alteration pitch))))))

(define-public (write-me message x)
  (write message) (write x) (newline) x)

(define-public (double-plus-new-chord->markup-banter . args)
  (apply double-plus-new-chord->markup (cons 'banter args)))

(define-public (double-plus-new-chord->markup-jazz . args)
  (apply double-plus-new-chord->markup (cons 'jazz args)))

(define-public (double-plus-new-chord->markup
		func pitches bass inversion options)
  "Entry point for New_chord_name_engraver.  See
double-plus-new-chord-name.scm for the signature of FUNC.  PITCHES,
BASS and INVERSION are lily pitches.  OPTIONS is an alist-alist (see
input/test/dpncnt.ly).
 "

      
  (define (step-nr pitch)
    (let* ((pitch-nr (+ (* 7 (ly:pitch-octave pitch))
			(ly:pitch-notename pitch)))
	   (tonic-nr (+ (* 7 (ly:pitch-octave (car pitches)))
			(ly:pitch-notename (car pitches)))))
      (+ 1 (- pitch-nr tonic-nr))))
    
  (define (next-third pitch)
    (ly:pitch-transpose pitch
			(ly:make-pitch 0 2 (if (or (= (step-nr pitch) 3)
						   (= (step-nr pitch) 5))
					       -1 0))))

  (define (step-alteration pitch)
    (let* ((diff (ly:pitch-diff (ly:make-pitch 0 0 0) (car pitches)))
	   (normalized-pitch (ly:pitch-transpose pitch diff))
	   (alteration (ly:pitch-alteration normalized-pitch)))
      (if (= (step-nr pitch) 7) (+ alteration 1) alteration)))
    
  (define (pitch-unalter pitch)
    (let ((alteration (step-alteration pitch)))
      (if (= alteration 0)
	  pitch
	  (ly:make-pitch (ly:pitch-octave pitch) (ly:pitch-notename pitch)
			 (- (ly:pitch-alteration pitch) alteration)))))

  (define (step-even-or-altered? pitch)
    (let ((nr (step-nr pitch)))
      (if (!= (modulo nr 2) 0)
	  (!= (step-alteration pitch) 0)
	  #t)))

  (define (step->markup-plusminus pitch)
    (make-line-markup
     (list
      (make-simple-markup (number->string (step-nr pitch)))
      (make-simple-markup
       (case (step-alteration pitch)
	 ((-2) "--")
	 ((-1) "-")
	 ((0) "")
	 ((1) "+")
	 ((2) "++"))))))
  
  (define (step->markup-accidental pitch)
    (make-line-markup
     (list
      (accidental->markup (step-alteration pitch))
      (make-simple-markup (number->string (step-nr pitch))))))
  
  (define (sub->markup pitch)
    ;;(make-line-markup (list (make-simple-markup "no") (step->markup pitch))))
    ;; urg
    (make-line-markup (list (make-simple-markup "no")
			    (step->markup-plusminus pitch))))
    
			 
  (define (get-full-list pitch)
    (if (< (step-nr pitch) (step-nr (tail pitches)))
	(cons pitch (get-full-list (next-third pitch)))
	'()))

  (define (get-consecutive nr pitches)
    (if (pair? pitches)
	(let* ((pitch-nr (step-nr (car pitches)))
	       (next-nr (if (!= (modulo pitch-nr 2) 0) (+ pitch-nr 2) nr)))
	  (if (<= pitch-nr nr)
	      (cons (car pitches) (get-consecutive next-nr (cdr pitches)))
	      '()))
	'()))

  (let* ((all pitches)
	 (highest (tail all))
	 (full (get-full-list (car all)))
	 (missing (list-minus full (map pitch-unalter all)))
	 (consecutive (get-consecutive 1 all))
	 (rest (list-minus all consecutive))
	 (altered (filter-list step-even-or-altered? all))
	 (cons-alt (filter-list step-even-or-altered? consecutive))
	 (base (list-minus consecutive altered))
	 
	 (full-exceptions (assoc 'full-exceptions options))
	 (partial-exceptions (assoc 'partial-exceptions options)))

    ;;(newline)
    ;;(write-me "pitches" pitches)
    ;;(write-me "altered:" altered)
    ;;(write-me "missing:" missing)
    ;;(write-me "consecutive:" consecutive)
    ;;(write-me "rest:" rest)

    (case func
      ((banter)
       ;;    tonic
       ;;    + steps:altered + (highest all -- if not altered)
       ;;    + subs:missing
       
       (let* ((tonic->markup
	       (assoc-default 'tonic->markup options pitch->markup))
	      (step->markup
	       (assoc-default 'step->markup options step->markup-plusminus))
	      (sep
	       (assoc-default 'separator options (make-simple-markup "/"))))
	 
	 (make-line-markup
	  (list
	   (tonic->markup (car pitches))

	   (make-normal-size-super-markup
	    (markup-join
	     (apply append
		    (map step->markup
			 (append altered
				 (if (and (> (step-nr highest) 5)
					  (not
					   (step-even-or-altered? highest)))
				     (list highest) '())))
		    
		    (list (map sub->markup missing)))
	     sep))))))

      
      ((jazz)
       ;;    tonic
       ;;    + steps:(highest base) + cons-alt
       ;;    + 'add'
       ;;    + steps:rest
       (let* ((tonic->markup
	       (assoc-default 'tonic->markup options pitch->markup))
	      (step->markup
	       (assoc-default 'step->markup options step->markup-accidental))
	      (sep
	       (assoc-default 'separator options (make-simple-markup " ")))
	      (add-prefix
	       (assoc-default 'add-prefix options
			      (make-simple-markup " add"))))
	 
	 (make-line-markup
	  (list
	   (tonic->markup (car pitches))
	   
	   (make-normal-size-super-markup
	    (make-line-markup
	     (list
	      (markup-join (map step->markup (cons (tail base) cons-alt)) sep)
	      (if (pair? rest)
		  add-prefix
		  empty-markup)
	      (markup-join (map step->markup rest) sep))))))))
      
      (else empty-markup))))
  
