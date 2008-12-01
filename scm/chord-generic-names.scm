;;;; chord-generic-names.scm -- Compile chord names
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;;
;;;; (c) 2003--2008 Jan Nieuwenhuizen <janneke@gnu.org>


;;;; NOTE: this is experimental code
;;;; Base and inversion are ignored.
;;;; Naming of the base chord (steps 1-5) is handled by exceptions only
;;;; see input/test/chord-names-dpnj.ly

(define (markup-or-empty-markup markup)
  "Return MARKUP if markup, else empty-markup"
  (if (markup? markup) markup empty-markup))

(define (conditional-kern-before markup bool amount)
  "Add AMOUNT of space before MARKUP if BOOL is true."
  (if bool
      (make-line-markup
       (list (make-hspace-markup amount)
	     markup))
      markup))

(define-public (banter-chord-names pitches bass inversion context)
  (ugh-compat-double-plus-new-chord->markup
   'banter pitches bass inversion context '()))

(define-public (jazz-chord-names pitches bass inversion context)
  (ugh-compat-double-plus-new-chord->markup
   'jazz pitches bass inversion context '()))

(define-public (ugh-compat-double-plus-new-chord->markup
		style pitches bass inversion context options)
  "Entry point for New_chord_name_engraver.

FIXME: func, options/context have changed
 See
double-plus-new-chord-name.scm for the signature of STYLE.  PITCHES,
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
					       FLAT 0))))

  (define (step-alteration pitch)
    (let* ((diff (ly:pitch-diff (ly:make-pitch 0 0 0) (car pitches)))
	   (normalized-pitch (ly:pitch-transpose pitch diff))
	   (alteration (ly:pitch-alteration normalized-pitch)))
      (if (= (step-nr pitch) 7) (+ alteration SEMI-TONE) alteration)))

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
	 ((DOUBLE-FLAT) "--")
	 ((FLAT) "-")
	 ((NATURAL) "")
	 ((SHARP) "+")
	 ((DOUBLE-SHARP) "++"))))))

  (define (step->markup-accidental pitch)
    (make-line-markup
     (list (accidental->markup (step-alteration pitch))
	   (make-simple-markup (number->string (step-nr pitch))))))

  (define (step->markup-ignatzek pitch)
    (make-line-markup
     (if (and (= (step-nr pitch) 7)
	      (= (step-alteration pitch) 1))
	 (list (ly:context-property context 'majorSevenSymbol))
	 (list (accidental->markup (step-alteration pitch))
	       (make-simple-markup (number->string (step-nr pitch)))))))
	
  ;; tja, kennok
  (define (make-sub->markup step->markup)
    (lambda (pitch)
      (make-line-markup (list (make-simple-markup "no")
			      (step->markup pitch)))))
			
  (define (step-based-sub->markup step->markup pitch)
    (make-line-markup (list (make-simple-markup "no") (step->markup pitch))))
			
  (define (get-full-list pitch)
    (if (<= (step-nr pitch) (step-nr (last pitches)))
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
	#f))

  (define (partial-match exceptions)
    (if (pair? exceptions)
	(let* ((e (car exceptions))
	       (e-pitches (car e)))
	  (if (equal? e-pitches (take pitches (length e-pitches)))
	      e
	      (partial-match (cdr exceptions))))
	#f))

  (if #f (begin
	   (write-me "pitches: " pitches)))
  (let* ((full-exceptions
	  (ly:context-property context 'chordNameExceptionsFull))
	 (full-exception (full-match full-exceptions))
	 (full-markup (if full-exception (cadr full-exception) '()))
	 (partial-exceptions
	  (ly:context-property context 'chordNameExceptionsPartial))
	 (partial-exception (partial-match partial-exceptions))
	 (partial-pitches (if partial-exception (car partial-exception) '()))
	 (partial-markup-prefix
	  (if partial-exception (markup-or-empty-markup
				 (cadr partial-exception)) empty-markup))
	 (partial-markup-suffix
	  (if (and partial-exception (pair? (cddr partial-exception)))
	      (markup-or-empty-markup (caddr partial-exception)) empty-markup))
	 (root (car pitches))
	 (full (get-full-list root))
	 ;; kludge alert: replace partial matched lower part of all with
	 ;; 'normal' pitches from full
	 ;; (all pitches)
	 (all (append (take full (length partial-pitches))
		      (drop pitches (length partial-pitches))))
	
	 (highest (last all))
	 (missing (list-minus full (map pitch-unalter all)))
	 (consecutive (get-consecutive 1 all))
	 (rest (list-minus all consecutive))
	 (altered (filter step-even-or-altered? all))
	 (cons-alt (filter step-even-or-altered? consecutive))
	 (base (list-minus consecutive altered)))
	

    (if #f (begin
	     (write-me "full:" full)
	      ;; (write-me "partial-pitches:" partial-pitches)
	      (write-me "full-markup:" full-markup)
	      (write-me "partial-markup-perfix:" partial-markup-prefix)
	      (write-me "partial-markup-suffix:" partial-markup-suffix)
	      (write-me "all:" all)
	      (write-me "altered:" altered)
	      (write-me "missing:" missing)
	      (write-me "consecutive:" consecutive)
	      (write-me "rest:" rest)
	      (write-me "base:" base)))

    (case style
      ((banter)
       ;;    root
       ;;    + steps:altered + (highest all -- if not altered)
       ;;    + subs:missing

       (let* ((root->markup (assoc-get
			      'root->markup options note-name->markup))
	      (step->markup (assoc-get
			     'step->markup options step->markup-plusminus))
	      (sub->markup (assoc-get
			    'sub->markup options
			    (lambda (x)
			      (step-based-sub->markup step->markup x))))
	      (sep (assoc-get
		    'separator options (make-simple-markup "/"))))
	
	 (if
	  (pair? full-markup)
	  (make-line-markup (list (root->markup root) full-markup))
	
	  (make-line-markup
	   (list
	    (root->markup root)
	    partial-markup-prefix
	    (make-normal-size-super-markup
	     (markup-join
	      (apply append
		     (map step->markup
			  (append altered
				  (if (and (> (step-nr highest) 5)
					   (not
					    (step-even-or-altered? highest)))
				      (list highest) '())))
		      (list partial-markup-suffix)
		     (list (map sub->markup missing)))
	      sep)))))))


      ((jazz)
       ;;    root
       ;;    + steps:(highest base) + cons-alt
       ;;    + 'add'
       ;;    + steps:rest
       (let* ((root->markup (assoc-get
			      'root->markup options note-name->markup))
	      (step->markup
	       (assoc-get
		;; FIXME: ignatzek
		;;'step->markup options step->markup-accidental))
		'step->markup options step->markup-ignatzek))
	      (sep (assoc-get
		    'separator options (make-simple-markup " ")))
	      (add-prefix (assoc-get 'add-prefix options
					     (make-simple-markup " add"))))
	
	 (if
	  (pair? full-markup)
	  (make-line-markup (list (root->markup root) full-markup))
	
	  (make-line-markup
	   (list
	    (root->markup root)
	    partial-markup-prefix
	    (make-normal-size-super-markup
	     (make-line-markup
	      (list
	
	       ;; kludge alert: omit <= 5
	       ;;(markup-join (map step->markup
	       ;;			 (cons (last base) cons-alt)) sep)
	
	       ;; This fixes:
	       ;;  c     C5       -> C
	       ;;  c:2   C5 2     -> C2
	       ;;  c:3-  Cm5      -> Cm
	       ;;  c:6.9 C5 6add9 -> C6 add 9 (add?)
	       ;;  ch = \chords { c c:2 c:3- c:6.9^7 }
	       (markup-join (map step->markup
  				 (let ((tb (last base)))
  				   (if (> (step-nr tb) 5)
  				       (cons tb cons-alt)
  				       cons-alt))) sep)
	
	       (if (pair? rest)
		   add-prefix
		   empty-markup)
	       (markup-join (map step->markup rest) sep)
	       partial-markup-suffix))))))))

       (else empty-markup))))
