;;; chord.scm -- to be included in/to replace chord-name.scm
;;; 2000 janneke@gnu.org
;;;

(use-modules
   (ice-9 debug)
   ;; urg, these two only to guess if a '/' is needed to separate
   ;; user-chord-name and additions/subtractions
   (ice-9 format)
   (ice-9 regex)
   )

;; The regex module may not be available, or may be broken.
(define use-regex
  (let ((os (string-downcase (vector-ref (uname) 0))))
    (not (equal? "cygwin" (substring os 0 (min 6 (string-length os)))))))

;;
;; (octave notename accidental)
;;

;;
;; text: list of word
;; word: string + optional list of property
;; property: size, style, font, super, offset
;;

;; TODO
;;
;; * clean split of base/banter/american stuff
;; * text definition is rather ad-hoc
;; * do without format module
;; * finish and check american names
;; * make notename (tonic) configurable from mudela
;; * fix append/cons stuff in inner-name-banter
;;


;;;;;;;;;
(define chord::names-alist-banter '())
(set! chord::names-alist-banter
      (append 
	'(
	; C iso C.no3.no5
	(((0 . 0)) . #f)
	; C iso C.no5
	(((0 . 0) (2 . 0)) . #f)
	; Cm iso Cm.no5
	(((0 . 0) (2 . -1)) . ("m"))
	; C2 iso C2.no3
	(((0 . 0) (1 . 0) (4 . 0)) . (("2" (type . "super"))))
	; C4 iso C4.no3
	(((0 . 0) (3 . 0) (4 . 0)) . (("4" (type . "super"))))
	; Cdim iso Cm5-
	(((0 . 0) (2 . -1) (4 . -1)) . ("dim"))
	; Co iso Cm5-7-
	; urg
        (((0 . 0) (2 . -1) (4 . -1) (6 . -2)) . (("o" (type . "super"))))
	; Cdim9
	(((0 . 0) (2 . -1) (4 . -1) (6 . -2) (1 . -1)) . ("dim" ("9" (type . "super"))))
	(((0 . 0) (2 . -1) (4 . -1) (6 . -2) (1 . -1) (3 . -1)) . ("dim" ("11" (type . "super"))))
	)
      chord::names-alist-banter))


(define chord::names-alist-american '())
(set! chord::names-alist-american
      (append 
       '(
	 (((0 . 0)) . #f)
	 (((0 . 0) (2 . 0)) . #f)
	 (((0 . 0) (2 . -1)) . ("m"))
	 (((0 . 0) (2 . -1) (4 . -1)) . ("dim"))
	 (((0 . 0) (4 . 0)) . (("5" (type . "super"))))
	 (((0 . 0) (3 . 0) (4 . 0)) . ("sus"))
	 (((0 . 0) (2 . -1) (4 . -1)) . (("o" (type . "super"))))

	 (((0 . 0) (2 . -1) (4 . -1) (6 . -2)) . (("o7" (type . "super"))))
	 ;jazz: the delta, see jazz-chords.ly
	 ;(((0 . 0) (2 . -1) (4 . -1) (6 . -2)) .  (("N" (type . "super") (style . "msam") (size . -3))))

	 ;(((0 . 0) (2 . -1) (4 . -1) (6 . -1)) . (("x7" (type . "super"))))
	 ; slashed o
	 (((0 . 0) (2 . -1) (4 . -1) (6 . -1)) . (("o" (type . "super")) ("/" (size . -2) (offset . (-0.58 . 0.5))) ("7" (type . "super"))))

	 (((0 . 0) (2 . 0) (4 . 1)) . ("aug"))
	 (((0 . 0) (2 . 0) (4 . 1) (6 . -1)) . (("aug" ("7" (type . "super")))))

	 (((0 . 0) (2 . 0) (4 . -1) (6 . 0)) . (("maj7" (type . "super")) ("accidentals--1" (font . "feta") (type . "super")) ("5" (type . "super"))))
	  
	 (((0 . 0) (3 . 0) (4 . 0) (6 . -1)) . (("7sus4" (type . "super"))))

	 (((0 . 0) (2 . 0) (4 . 0) (5 . 0)) . (("maj6" (type . "super"))))
	 ;; dont need this?
	 ;(((0 . 0) (2 . -1) (4 . 0) (5 . 0)) . ("m6" . ""))

	 ;; c = 0, d = 1
	 ;;(((0 . 0) (2 . 0) (4 . 0) (8 . 0)) . ("add9" . ""))
	 ;;(((0 . 0) (2 . 0) (4 . 0) (1 . 0)) . ("" . (("script" . "add9"))))

	 ;; we don't want the '/no7'
	 ;;(((0 . 0) (2 . 0) (4 . 0) (5 . 0) (8 . 0)) . ("6/9" . ""))
	 ;;(((0 . 0) (2 . 0) (4 . 0) (5 . 0) (1 . 0)) . (("script" . "6/9"))))

	 ;;already have this?
	 ;;(((0 . 0) (2 . 0) (4 . 0) (6 . 0) (1 . 0)) . ("maj9" . ""))

	 )
      chord::names-alist-american))

;;;;;;;;;;


(define (pitch->note-name pitch)
  (cons (cadr pitch) (caddr pitch)))
  
(define (pitch->text pitch)
  (cons
    (make-string 1 (integer->char (+ (modulo (+ (cadr pitch) 2) 7) 65)))
    (if (= (caddr pitch) 0)
      '()
      (list (list (string-append "accidentals-" 
				 (number->string (caddr pitch)))
		  '(font . "feta"))))))

(define (step->text pitch)
  (string-append
    (number->string (+ (cadr pitch) (if (= (car pitch) 0) 1 8)))
    (case (caddr pitch)
      ((-2) "--")
      ((-1) "-")
      ((0) "")
      ((1) "+")
      ((2) "++"))))

(define (pitch->text-banter pitch)
  (pitch->text pitch))
  
(define (step->text-banter pitch)
  (if (= (cadr pitch) 6)
      (case (caddr pitch)
	((-2) "7-")
	((-1) "7")
	((0) "maj7")
	((1) "7+")
	((2) "7+"))
      (step->text pitch)))

(define pitch::semitone-vec (list->vector '(0 2 4 5 7 9 11)))

(define (pitch::semitone pitch)
  (+ (* (car pitch) 12) 
     (vector-ref pitch::semitone-vec (modulo (cadr pitch) 7)) 
     (caddr pitch)))

(define (pitch::transpose pitch delta)
  (let ((simple-octave (+ (car pitch) (car delta)))
	(simple-notename (+ (cadr pitch) (cadr delta))))
    (let ((octave (+ simple-octave (quotient simple-notename 7)))
	   (notename (modulo simple-notename 7)))
      (let ((accidental (- (+ (pitch::semitone pitch) (pitch::semitone delta))
			   (pitch::semitone `(,octave ,notename 0)))))
	`(,octave ,notename ,accidental)))))
    
(define (pitch::diff pitch tonic)
  (let ((simple-octave (- (car pitch) (car tonic)))
	(simple-notename (- (cadr pitch) (cadr tonic))))
    (let ((octave (+ simple-octave (quotient simple-notename 7)
		     (if (< simple-notename 0) -1 0)))
	  (notename (modulo simple-notename 7)))
      (let ((accidental (- (pitch::semitone pitch)
			  (pitch::semitone tonic) 
			  (pitch::semitone `(,octave ,notename 0)))))
	`(,octave ,notename ,accidental)))))

(define (pitch::note-pitch pitch)
  (+ (* (car pitch) 7) (cadr pitch)))

(define (chord::step tonic pitch)
 (- (pitch::note-pitch pitch) (pitch::note-pitch tonic)))

;; text: list of word
;; word: string + optional list of property
;; property: align, kern, font (?), size

(define chord::minor-major-vec (list->vector '(0 -1 -1 0 -1 -1 0)))

;; compute the relative-to-tonic pitch that goes with 'step'
(define (chord::step-pitch tonic step)
  ;; urg, we only do this for thirds
  (if (= (modulo step 2) 0)
    '(0 0 0)
    (let loop ((i 1) (pitch tonic))
      (if (= i step) pitch
	(loop (+ i 2) 
	      (pitch::transpose 
		pitch `(0 2 ,(vector-ref chord::minor-major-vec 
		;; -1 (step=1 -> vector=0) + 7 = 6
		(modulo (+ i 6) 7)))))))))

;; find the pitches that are not part of `normal' chord
(define (chord::additions chord-pitches)
  (let ((tonic (car chord-pitches)))
    ;; walk the chord steps: 1, 3, 5
    (let loop ((step 1) (pitches chord-pitches) (additions '()))
      (if (pair? pitches)
	(let* ((pitch (car pitches))
	       (p-step (+ (- (pitch::note-pitch pitch)
			     (pitch::note-pitch tonic))
			  1)))
	  ;; pitch is an addition if 
	  (if (or 
		;; it comes before this step or
		(< p-step step)
		;; its step is even or
		(= (modulo p-step 2) 0)
		;; has same step, but different accidental or
		(and (= p-step step)
		     (not (equal? pitch (chord::step-pitch tonic step))))
		;; is the last of the chord and not one of base thirds
		(and (> p-step  5)
		     (= (length pitches) 1)))
	    (loop step (cdr pitches) (cons pitch additions))
	  (if (= p-step step)
	    (loop step (cdr pitches) additions)
	    (loop (+ step 2) pitches additions))))
      (reverse additions)))))

;; find the pitches that are missing from `normal' chord
(define (chord::subtractions chord-pitches)
  (let ((tonic (car chord-pitches)))
    (let loop ((step 1) (pitches chord-pitches) (subtractions '()))
      (if (pair? pitches)
	(let* ((pitch (car pitches))
	       (p-step (+ (- (pitch::note-pitch pitch)
			     (pitch::note-pitch tonic))
			  1)))
	  ;; pitch is an subtraction if 
	  ;; a step is missing or
	  (if (> p-step step)
	    (loop (+ step 2) pitches
		(cons (chord::step-pitch tonic step) subtractions))
	  ;; there are no pitches left, but base thirds are not yet done and
	  (if (and (<= step 5)
		   (= (length pitches) 1))
	    ;; present pitch is not missing step
	    (if (= p-step step)
	      (loop (+ step 2) pitches subtractions)
	      (loop (+ step 2) pitches 
		    (cons (chord::step-pitch tonic step) subtractions)))
	    (if (= p-step step)
	      (loop (+ step 2) (cdr pitches) subtractions)
	      (loop step (cdr pitches) subtractions)))))
	(reverse subtractions)))))

;; combine tonic, user-specified chordname,
;; additions, subtractions and base or inversion to chord name
;;
(define (chord::inner-name-banter tonic user-name additions subtractions base-and-inversion)
    (apply append (pitch->text-banter tonic)
	   (if user-name user-name '())
	   ;; why does list->string not work, format seems only hope...
	   (if (and use-regex
		    (string-match "super" (format "~s" user-name))
		    (or (pair? additions)
			(pair? subtractions)))
	       '(("/" (type . "super")))
	       '())
	   (let loop ((from additions) (to '()))
	     (if (pair? from)
		 (let ((p (car from)))
		   (loop (cdr from) 
			 (append to
			  (cons
			   (cons (step->text-banter p) '((type . "super")))
			   (if (or (pair? (cdr from))
				   (pair? subtractions))
			       '(("/" (type . "super")))
			       '())))))
		 to))
	   (let loop ((from subtractions) (to '()))
	     (if (pair? from)
		 (let ((p (car from)))
		   (loop (cdr from) 
			 (append to
			   (cons '("no" (type . "super"))
				 (cons
				  (cons (step->text-banter p) '((type . "super")))
					    (if (pair? (cdr from))
						'(("/" (type . "super")))
						'()))))))
		 to))
	   (if (and (pair? base-and-inversion)
		    (or (car base-and-inversion)
			(cdr base-and-inversion)))
	       (cons "/" (append
			  (if (car base-and-inversion)
			      (pitch->text 
			       (car base-and-inversion))
			      (pitch->text 
			       (cdr base-and-inversion)))
			  '()))
	       '())
	   '()))

(define (chord::name-banter tonic user-name pitches base-and-inversion)
  (let ((additions (chord::additions pitches))
	(subtractions (chord::subtractions pitches)))
    (chord::inner-name-banter tonic user-name additions subtractions base-and-inversion)))

;; american chordnames use no "no",
;; but otherwise very similar to banter for now
(define (chord::name-american tonic user-name pitches base-and-inversion)
  (let ((additions (chord::additions pitches))
	(subtractions #f))
    (chord::inner-name-banter tonic user-name additions subtractions base-and-inversion)))

;; C++ entry point
;; 
;; Check for each subset of chord, full chord first, if there's a
;; user-override.  Split the chord into user-overridden and to-be-done
;; parts, complete the missing user-override matched part with normal
;; chord to be name-calculated.
;;
(define (chord::user-name style pitches base-and-inversion)
  ;(display "pitches:") (display  pitches) (newline)
  ;(display "style:") (display  style) (newline)
  ;(display "b&i:") (display  base-and-inversion) (newline)
  (let ((diff (pitch::diff '(0 0 0) (car pitches)))
	(name-func 
	  (eval (string->symbol (string-append "chord::name-" style))))
  	(names-alist 
	  (eval (string->symbol (string-append "chord::names-alist-" style)))))
  (let loop ((note-names (reverse pitches))
	     (chord '())
	     (user-name #f))
    (if (pair? note-names)
      (let ((entry (assoc 
		     (reverse 
		       (map (lambda (x) 
			      (pitch->note-name (pitch::transpose x diff)))
			    note-names))
		     names-alist)))
	(if entry
	  ;; urg? found: break loop
	  (loop '() chord (cdr entry))
	  (loop (cdr note-names) (cons (car note-names) chord) #f)))
      (let* ((transposed (if pitches 
			   (map (lambda (x) (pitch::transpose x diff)) chord)
			   '()))
	     (matched (if (= (length chord) 0)
			  3
			  (- (length pitches) (length chord))))
	     (completed 
	      (append (do ((i matched (- i 1))
			   (base '() (cons `(0 ,(* (- i 1) 2) 0) base)))
			   ((= i 0) base)
			   ())
		  transposed)))
      (name-func (car pitches) user-name completed base-and-inversion))))))

