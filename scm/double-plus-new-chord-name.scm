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
;;  (display message) (write x) (newline) x)
  x)

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


(define (split-at predicate l)
 "Split L = (a_1 a_2 ... a_k b_1 ... b_k)
into L1 = (a_1 ... a_k ) and L2 =(b_1 .. b_k) 
Such that (PREDICATE a_i a_{i+1}) and not (PREDICATE a_k b_1).
L1 is copied, L2 not.

(split-at (lambda (x y) (= (- y x) 2))  '(1 3 5 9 11) (cons '() '()))"
;; "

;; KUT EMACS MODE.

  (define (inner-split predicate l acc)
  (cond
   ((null? l) acc)
   ((null? (cdr l))
    (set-car! acc (cons (car l) (car acc)))
    acc)
   ((predicate (car l) (cadr l))
    (set-car! acc (cons (car l) (car acc)))
    (inner-split predicate (cdr l) acc))
   (else
    (set-car! acc (cons (car l) (car acc)))
    (set-cdr! acc (cdr l))
    acc)

  ))
 (let*
    ((c (cons '() '()))
     )
  (inner-split predicate l  c)
  (set-car! c (reverse! (car c))) 
  c)
)

;; MARKUP functions
(define (markup-join markups sep)
  "Return line-markup of MARKUPS, joining them with markup SEP"
  (if (pair? markups)
      (make-line-markup (list-insert-separator markups sep))
      empty-markup))

(define (markup-or-empty-markup markup)
  "Return MARKUP if markup, else empty-markup"
  (if (markup? markup) markup empty-markup))


;; Generic PITCH/MARKUP functions
(define (ly:pitch-diff pitch root)
  "Return pitch with value DELTA =  PITCH - ROOT, ie,
ROOT == (ly:pitch-transpose root delta)."


  ;; kludgy. Do this in C++ ? --hwn
  
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


(define (conditional-kern-before markup bool amount)
  "Add AMOUNT of space before MARKUP if BOOL is true."
  (if bool
      (make-line-markup
       (list (make-hspace-markup amount)
	     markup))
      markup
      ))
  
(define (accidental->markup alteration)
  "Return accidental markup for ALTERATION."
  (if (= alteration 0)
      (make-line-markup (list empty-markup))
      (conditional-kern-before
       (make-smaller-markup
	(make-raise-markup
	(if (= alteration -1)
	    0.3
	    0.6)
	(make-musicglyph-markup
	 (string-append "accidentals-" (number->string alteration)))))
       (= alteration -1) 0.2
       )))

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
;; The idea is: split chords into
;;  
;;  ROOT PREFIXES MAIN-NAME ALTERATIONS SUFFIXES ADDITIONS
;;
;; and put that through a layout routine.
;; 
;; the split is a procedural process , with lots of set!. 
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

  (define maj7-markup
    (make-simple-markup "maj7")
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
    (let*
	(
	 (num-markup (make-simple-markup
		      (number->string (pitch-step pitch))))
	 (args (list num-markup))
	 (total (if (= (ly:pitch-alteration pitch) 0)
		    (if (= (pitch-step pitch) 7)
			(list maj7-markup)
			args)
		    (cons (accidental->markup (step-alteration pitch)) args)
		    ))
	 
	 )
      
    (make-line-markup total)))

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

  (define (pitch-step p)
    "Musicological notation for an interval. Eg. C to D is 2."
    (+ 1 (ly:pitch-steps p)))
  
  (define (glue-word-to-step word x)
    (make-line-markup 
     (list
      (make-simple-markup word)
      (name-step x)))
    )

  (define (is-natural-alteration? p)
    (= (assoc-get-default (pitch-step p) natural-chord-alterations 0) (ly:pitch-alteration p))
    )
  
  (define (filter-main-name p)
    "The main name: don't print anything for natural 5 or 3."
    (if
     (and (is-natural-alteration? p)
	  (or (= (pitch-step p) 5)
	      (= (pitch-step p) 3)))
     '()
     (list (name-step p))
    ))


  (define (ignatzek-format-chord-name
	   root
	   prefix-modifiers
	   main-name
	   alteration-pitches
	   addition-pitches
	   suffix-modifiers
	   )

    
    (define (suffix-modifier->markup mod)
      (if (or (= 4 (pitch-step mod))
	      (= 2 (pitch-step mod)))
	  (glue-word-to-step "sus" mod)
	  (glue-word-to-step "huh" mod)
	  ))
    
    (define (prefix-modifier->markup mod)
      (if (and (= 3 (pitch-step mod))
	       (= -1 (ly:pitch-alteration mod)))
	  (make-simple-markup "m")
	  (make-simple-markup "huh")
	  ))

    
    (define (filter-alterations alters)
      (define (altered? p)
	(not (is-natural-alteration? p)))
      
      (if
       (null? alters)
       '()
       (let*
	   (
	    (l (filter-list altered? alters))
	    (lp (last-pair alters))
	    )

	 ;; we want the highest also if unaltered
	 (if (and (not (altered? (car lp)))
		  (> (pitch-step (car lp)) 5))
	     (append l (last-pair alters))
	     l)
	 )))
    
    (let*
	(
	 (sep (make-simple-markup "/"))
	 (root-markup (pitch->markup root))
	 (add-markups (map (lambda (x)
			     (glue-word-to-step "add" x))
			   addition-pitches))
	 (filtered-alterations (filter-alterations alteration-pitches))
	 (alterations (map name-step filtered-alterations))
	 (suffixes (map suffix-modifier->markup suffix-modifiers))
	 (prefixes (map prefix-modifier->markup prefix-modifiers))
	 (prefix-markup (markup-join prefixes sep))
	 (main-markups (filter-main-name main-name))
	 (to-be-raised-stuff (markup-join
			      (append
			       main-markups
			       alterations
			       suffixes
			       add-markups) sep))
	 )
      (make-line-markup
       (list
	root-markup
	prefix-markup
	(make-super-markup to-be-raised-stuff))
       )))
  

  (let*
     (
      (root (car in-pitches))
      (pitches (map (lambda (x) (ly:pitch-diff x root)) (cdr in-pitches)))
      (prefixes '())
      (suffixes '())
      (add-steps '())
      (main-name #f)
      (alterations '())
      )
   
  ;; handle sus4 suffix.
  (if (get-step 4 pitches)
      (begin
	(if (get-step 3 pitches)
	    (begin
	      (set! add-steps (cons (get-step 3 pitches) add-steps))
	      (set! pitches (remove-step 3 pitches))
	    ))
	(set! suffixes  (cons (get-step 4 pitches) suffixes))
      )
  )

  ;; handle sus2 suffix.
  ;; ugh - dup, should use loop.
  (if (get-step 2 pitches)
      (begin
	(if (get-step 3 pitches)
	    (begin
	      (set! add-steps (cons (get-step 3 pitches) add-steps))
	      (set! pitches (remove-step 3 pitches))
	    ))
	(set! suffixes  (cons (get-step 2 pitches) suffixes))
      )
  )

  (if (and (get-step 3 pitches)
	   (= (ly:pitch-alteration (get-step 3 pitches)) -1))
      (set! prefixes (cons (get-step 3 pitches) prefixes))
      )


  ;; lazy bum. Should write loop.
  (cond
   ((get-step 7 pitches) (set! main-name (get-step 7 pitches)))
   ((get-step 6 pitches) (set! main-name (get-step 6 pitches)))
   ((get-step 5 pitches) (set! main-name (get-step 5 pitches)))
   ((get-step 4 pitches) (set! main-name (get-step 4 pitches)))
   ((get-step 3 pitches) (set! main-name (get-step 3 pitches)))
   )

  (let*
     (
      (3-diff? (lambda (x y)
		 (= (- (pitch-step y) (pitch-step x)) 2)))
      (split (split-at 3-diff? (remove-uptil-step 5 pitches)))
      )
    (set! alterations (append alterations (car split)))
    (set! add-steps (append add-steps (cdr split)))
    
    (set! alterations (delq main-name alterations))
    (set! add-steps (delq main-name add-steps))


    ;; natural 5 7 9 11 13 etc. are named by the top pitch, without 
    ;; any alterations.
    (if (and
	 (= 7 (pitch-step main-name))
	 (is-natural-alteration? main-name)
	 (pair? (remove-uptil-step 7 alterations))
	 (reduce (lambda (x y) (and x y))
		      (map is-natural-alteration? alterations)))
	(begin
	  (set! main-name (tail alterations))
	  (set! alterations '())
	))

    
    (ignatzek-format-chord-name root prefixes main-name alterations add-steps suffixes)

    )
  
  ))

