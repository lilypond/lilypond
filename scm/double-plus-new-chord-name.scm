;;;; double-plus-new-chord-name.scm -- Compile chord names
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c) 2003 Jan Nieuwenhuizen <janneke@gnu.org>

;;;; NOTE: this is experimental code
;;;; Base and inversion are ignored.
;;;; Naming of the base chord (steps 1-5) is handled by exceptions only
;;;; see input/test/chord-names-dpnj.ly


(define-module (scm double-plus-new-chord-name))
(debug-enable 'backtrace)
(use-modules (ice-9 regex)
	     (ice-9 string-fun)
	     (ice-9 format)
	     (guile)
	     (lily))

(define this-module (current-module))


;; SCM utilily functions

(define (write-me message x)
  "Return X.  Display MESSAGE and write X.  Handy for debugging, possibly turned off."
  (display message) (write x) (newline) x)
;;  x)

(define (tail lst)
  "Return tail element of LST."
  (car (last-pair lst)))

(define (list-minus a b)
  "Return list of elements in A that are not in B."
  (if (pair? a)
      (if (pair? b)
	  (if (member (car a) b)
	      (list-minus (cdr a) b)
	      (cons (car a) (list-minus (cdr a) b)))
	  a)
      '()))

(define (first-n n lst)
  "Return first N elements of LST"
  (if (and (pair? lst)
	   (> n 0))
      (cons (car lst) (first-n (- n 1) (cdr lst)))
      '()))

(define (butfirst-n n lst)
  "Return all but first N entries of LST"
  (if (pair? lst)
      (if (> n 0)
	  (butfirst-n (- n 1) (cdr lst))
	  lst)
      '()))
  
(define (assoc-get key alist)
  "Return value if KEY in ALIST, else #f."
  (let ((entry (assoc key alist)))
    (if entry (cdr entry) #f)))
  
(define (assoc-get-default key alist default)
  "Return value if KEY in ALIST, else DEFAULT."
  (let ((entry (assoc key alist)))
    (if entry (cdr entry) default)))


;; MARKUP functions
(define (markup-join markups sep)
  "Return line-markup of MARKUPS, joining them with markup SEP"
  (if (pair? markups)
      (make-line-markup (reduce-list markups sep))
      empty-markup))

(define (markup-or-empty-markup markup)
  "Return MARKUP if markup, else empty-markup"
  (if (markup? markup) markup empty-markup))


;; Generic PITCH/MARKUP functions
(define (ly:pitch-diff pitch root)
  "Return pitch with value DELTA =  PITCH - ROOT, ie,
ROOT == (ly:pitch-transpose root delta)."


  ;; a little kludgy? Do this in C++ ? --hwn
  
  (let ((simple-octave (- (ly:pitch-octave pitch) (ly:pitch-octave root)))
	(simple-notename
	 (- (ly:pitch-notename pitch) (ly:pitch-notename root))))
    (let ((octave (+ simple-octave (quotient simple-notename 7)
		     (if (< simple-notename 0) -1 0)))
	  (notename (modulo simple-notename 7)))
      (let ((alteration
	     (- (ly:pitch-semitones pitch)
		(ly:pitch-semitones root) 
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
  "Return pitch markup for PITCH."
  (make-line-markup
   (list
    (make-simple-markup
     (vector-ref #("C" "D" "E" "F" "G" "A" "B") (ly:pitch-notename pitch)))
    (make-normal-size-super-markup
     (accidental->markup (ly:pitch-alteration pitch))))))

(define-public (double-plus-new-chord->markup-banter . args)
  (apply double-plus-new-chord->markup (cons 'banter args)))

(define-public (double-plus-new-chord->markup-jazz . args)
  (apply double-plus-new-chord->markup (cons 'jazz args)))

;; FIXME: if/when double-plus-new-chord->markup get installed
;; setting and calling can be done a bit handier.
(define-public (double-plus-new-chord->markup
		func root-markup pitches bass inversion options)
  "Entry point for New_chord_name_engraver.  See
double-plus-new-chord-name.scm for the signature of FUNC.  PITCHES,
BASS and INVERSION are lily pitches.  OPTIONS is an alist-alist (see
input/test/dpncnt.ly).
 "

      
  (define (step-nr pitch)
    (let* ((pitch-nr (+ (* 7 (ly:pitch-octave pitch))
			(ly:pitch-notename pitch)))
	   (root-nr (+ (* 7 (ly:pitch-octave (car pitches)))
			(ly:pitch-notename (car pitches)))))
      (+ 1 (- pitch-nr root-nr))))
    
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

  ;; tja, kennok
  (define (make-sub->markup step->markup)
    (lambda (pitch)
      (make-line-markup (list (make-simple-markup "no")
			      (step->markup pitch)))))
			 
  (define (step-based-sub->markup step->markup pitch)
    (make-line-markup (list (make-simple-markup "no") (step->markup pitch))))
			 
  (define (get-full-list pitch)
    (if (<= (step-nr pitch) (step-nr (tail pitches)))
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

  (define (full-match exceptions)
    (if (pair? exceptions)
	(let* ((e (car exceptions))
	       (e-pitches (car e)))
	  (if (equal? e-pitches pitches)
	      e
	      (full-match (cdr exceptions))))
	'(())))

  (define (partial-match exceptions)
    (if (pair? exceptions)
	(let* ((e (car exceptions))
	       (e-pitches (car e)))
	  (if (equal? e-pitches (first-n (length e-pitches) pitches))
	      e
	      (partial-match (cdr exceptions))))
	'(())))

  (write-me "options: " options)
  (write-me "pitches: " pitches)
  (let* ((full-exceptions (assoc-get 'full-exceptions options))
	 (full-exception (full-match full-exceptions))
	 (full-markup (cdr full-exception))
	 
	 (partial-exceptions (assoc-get 'partial-exceptions options))
	 (partial-exception (partial-match partial-exceptions))
	 (partial-pitches (car partial-exception))
	 (partial-markup (markup-or-empty-markup (cdr partial-exception)))

	 (root (car pitches))
	 (full (get-full-list root))
	 ;; kludge alert: replace partial matched lower part of all with
	 ;; 'normal' pitches from full
	 ;; (all pitches)
	 (all (append (first-n (length partial-pitches) full)
		      (butfirst-n (length partial-pitches) pitches)))
	      
	 (highest (tail all))
	 (missing (list-minus full (map pitch-unalter all)))
	 (consecutive (get-consecutive 1 all))
	 (rest (list-minus all consecutive))
	 (altered (filter-list step-even-or-altered? all))
	 (cons-alt (filter-list step-even-or-altered? consecutive))
	 (base (list-minus consecutive altered)))
	 

    (write-me "full:" full)
    ;; (write-me "partial-pitches:" partial-pitches)
    (write-me "full-markup:" full-markup)
    (write-me "partial-markup:" partial-markup)
    (write-me "all:" all)
    (write-me "altered:" altered)
    (write-me "missing:" missing)
    (write-me "consecutive:" consecutive)
    (write-me "rest:" rest)
    (write-me "base:" base)

    (case func
      ((banter)
       ;;    root
       ;;    + steps:altered + (highest all -- if not altered)
       ;;    + subs:missing
       
       (let* ((root->markup (assoc-get-default
			      'root->markup options pitch->markup))
	      (step->markup (assoc-get-default
			     'step->markup options step->markup-plusminus))
	      (sub->markup (assoc-get-default
			    'sub->markup options
			    (lambda (x)
			      (step-based-sub->markup step->markup x))))
	      (sep (assoc-get-default
		    'separator options (make-simple-markup "/"))))
	 
	 (if
	  (pair? full-markup)
	  (make-line-markup (list (root->markup root) full-markup))
	    
	  (make-line-markup
	   (list
	    (root->markup root)
	    partial-markup
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
	      sep)))))))
       
      
      ((jazz)
       ;;    root
       ;;    + steps:(highest base) + cons-alt
       ;;    + 'add'
       ;;    + steps:rest
       (let* ((root->markup (assoc-get-default
			      'root->markup options pitch->markup))
	      (step->markup (assoc-get-default
			     'step->markup options step->markup-accidental))
	      (sep (assoc-get-default
		    'separator options (make-simple-markup " ")))
	      (add-prefix (assoc-get-default 'add-prefix options
					     (make-simple-markup " add"))))
	 
	 (if
	  (pair? full-markup)
	  (make-line-markup (list (root->markup root) full-markup))
	  
	  (make-line-markup
	   (list
	    (root->markup root)
	    partial-markup
	    (make-normal-size-super-markup
	     (make-line-markup
	      (list
	       
	       ;; kludge alert: omit <= 5
	       ;;(markup-join (map step->markup
	       ;;			 (cons (tail base) cons-alt)) sep)
	       
	       ;; This fixes:
	       ;;  c     C5       -> C
	       ;;  c:2   C5 2     -> C2
	       ;;  c:3-  Cm5      -> Cm
	       ;;  c:6.9 C5 6add9 -> C6 add 9 (add?)
	       ;;  ch = \chords { c c:2 c:3- c:6.9^7 }
	       (markup-join (map step->markup
  				 (let ((tb (tail base)))
  				   (if (> (step-nr tb) 5)
  				       (cons tb cons-alt)
  				       cons-alt))) sep)
	       
	       (if (pair? rest)
		   add-prefix
		   empty-markup)
	       (markup-join (map step->markup rest) sep)))))))))
       
       (else empty-markup))))

  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; jazz-part 2
;;
;; after Klaus Ignatzek,   Die Jazzmethode fuer Klavier 1.
;; 


(define natural-chord-alterations
  '(
    (2 . 0)
    (3 . 0)
    (4 . 0)
    (5 . 0)
    (6 . 0)
    
    (7 . -1)
    (9 . 0)
    (11 . 0)
    (13 . 0))
    )

(define natural-7-up-alterations
  (butfirst-n 5 natural-chord-alterations) )

(define-public (ignatzek-chord-names
		in-pitches bass inversion options)

  (let*
     (
      (root (car in-pitches))
      (pitches (map (lambda (x) (ly:pitch-diff x root)) (cdr in-pitches)))
      (suffixes '())
      (add-steps '())
      (body '())
      (7-and-up '())
      (sequential-7-to-13 '())
      (sequential-count 0)
      (alterations '())
      (unaltered-count 0)
      )
  
  (define (get-step x ps)
    "Does PS have the X step? Return that step if it does."
    (if (null? ps)
	#f
	(if (= (- x 1) (ly:pitch-steps (car ps)))
	    (car ps) 
	    (get-step x (cdr ps)))
	))

  (define (name-step pitch)
    (define (step-alteration pitch)
      (- (ly:pitch-alteration pitch)
	 (assoc-get-default (+ 1  (ly:pitch-steps pitch)) natural-chord-alterations 0))
      )
      
    (make-line-markup
     (list
      (accidental->markup (step-alteration pitch))
      (make-simple-markup (number->string (+ 1 (ly:pitch-steps pitch)))))))

  
  (define (count-leading-true bs)
    "For the list of booleans BS, count with how many #t's it starts."
    (if (null? bs)
	0
	(if (car bs)
	    (+ 1 (count-leading-true (cdr bs)))
	    0)
    ))

  (define (remove-step x ps)
    "Copy PS, but leave out the Xth step."
    (if (null? ps)
	'()
	(let*
	    (
	     (t (remove-step x (cdr ps)))
	     )

	  (if (= (- x 1) (ly:pitch-steps (car ps)))
	       t
	       (cons (car ps) t)
	       ))
	   
    ))

  (define (remove-uptil-step x ps)
    "Copy PS, but leave out everything below the Xth step."
    (if (null? ps)
	'()
	(if (< (ly:pitch-steps (car ps)) (- x 1))
	    (remove-uptil-step x (cdr ps))
	    ps)
	)
    )



  (write-me "*****************\nchord " in-pitches)

  ;; handle sus4 suffix.
  (if (get-step 4 pitches)
      (begin
	(if (get-step 3 pitches)
	    (begin
	      (set! add-steps (cons (get-step 3 pitches) add-steps))
	      (set! pitches (remove-step 3 pitches))
	    ))
	(set! suffixes  (cons "sus4" suffixes))
      )
  )

  ;; handle sus2 suffix.
  (if (get-step 2 pitches)
      (begin
	(if (get-step 3 pitches)
	    (begin
	      (set! add-steps (cons (get-step 3 pitches) add-steps))
	      (set! pitches (remove-step 3 pitches))
	    ))
	(set! suffixes  (cons "sus2" suffixes))
      )
  )

  (if (and (get-step 3 pitches)
	   (= (ly:pitch-alteration (get-step 3 pitches)) -1))
      (set! body (cons "m" body))
      )

  (if (get-step 6 pitches)
      (set! body (cons "6" body ))
      )

  (if (>= (ly:pitch-steps (tail pitches))  6)
      (begin

	;; TODO: filter 6, 8, 10, 12, 14
	(set! 7-and-up (remove-uptil-step 7 pitches))
	(set! sequential-count
	      (count-leading-true
	       (map
		(lambda (x)
		  (get-step (car x) 7-and-up))
		natural-7-up-alterations
		)
	       ))

	(set! sequential-7-to-13
	      (first-n sequential-count  7-and-up))

	(set! add-steps (append add-steps
				(butfirst-n sequential-count 7-and-up)))

	(set! unaltered-count    
	      (count-leading-true
	       (map (lambda (x)
		      (= (ly:pitch-alteration (get-step (car x) sequential-7-to-13))
			 (cdr x)))
		    (first-n (length sequential-7-to-13) natural-7-up-alterations)
		    )))

	(write-me "sequential-7-to-13 " sequential-7-to-13)
	(if (pair? sequential-7-to-13)
	    (set! body
		  (cons (name-step
			 (list-ref sequential-7-to-13 (max 0 (- unaltered-count 1))))
			body)))

	(set! alterations (butfirst-n unaltered-count sequential-7-to-13))
	))
  
  (write-me "alterations " alterations)
  (write-me "add-steps " add-steps)
  (write-me "body " body)
  (write-me "suffixes " suffixes)

  
  (make-simple-markup "bla")
  
  ))

