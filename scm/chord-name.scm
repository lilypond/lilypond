;;;
;;; chord-name.scm -- Compile chord name
;;;
;;; source file of the GNU LilyPond music typesetter
;;; 
;;; (c) 2000 Jan Nieuwenhuizen <janneke@gnu.org>
;;;


(use-modules
   (ice-9 debug)
   ;; urg, these two only to guess if a '/' is needed to separate
   ;; user-chord-name and additions/subtractions
   (ice-9 format)
   (ice-9 regex)
   )

;;
;; (octave notename accidental)
;;

;;
;; text: scm markup text -- see font.scm and input/test/markup.ly
;;

;; TODO
;;
;; * clean split of base/banter/american stuff
;; * text definition is rather ad-hoc
;; * do without format module
;; * finish and check american names
;; * make notename (tonic) configurable from lilypond
;; * fix append/cons stuff in inner-name-banter
;; * doc strings.

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
	(((0 . 0) (1 . 0) (4 . 0)) . (super "2"))
	; C4 iso C4.no3
	(((0 . 0) (3 . 0) (4 . 0)) . (super "4"))
	; Cdim iso Cm5-
	(((0 . 0) (2 . -1) (4 . -1)) . ("dim"))
	; Co iso Cm5-7-
	; urg
        (((0 . 0) (2 . -1) (4 . -1) (6 . -2)) . (super "o"))
	; Cdim9
	(((0 . 0) (2 . -1) (4 . -1) (6 . -2) (1 . -1)) . ("dim" (super "9")))
	(((0 . 0) (2 . -1) (4 . -1) (6 . -2) (1 . -1) (3 . -1)) . ("dim" (super "11")))
	)
      chord::names-alist-banter))


;; NOTE: Duplicates of chord names defined elsewhere occur in this list
;; in order to prevent spurious superscripting of various chord names,
;; such as maj7, maj9, etc.
;;
;; See input/test/american-chords.ly
;;
;; James Hammons, <jlhamm@pacificnet.net>
;;

;; DONT use non-ascii characters, even if ``it works'' in Windows

(define chord::names-alist-american '())

(set! chord::names-alist-american
      (append 
       '(
	 (((0 . 0)) . #f)
	 (((0 . 0) (2 . 0)) . #f)
	 ;; Root-fifth chord
	 (((0 . 0) (4 . 0)) . ("5"))
	 ;; Common triads
	 (((0 . 0) (2 . -1)) . ("m"))
	 (((0 . 0) (3 . 0) (4 . 0)) . ("sus"))
	 (((0 . 0) (2 . -1) (4 . -1)) . ("dim"))
;Alternate:	 (((0 . 0) (2 . -1) (4 . -1)) . ((super "o")))
	 (((0 . 0) (2 . 0) (4 . 1)) . ("aug"))
;Alternate:	 (((0 . 0) (2 . 0) (4 . 1)) . ("+"))
	 (((0 . 0) (1 . 0) (4 . 0)) . ("2"))
	 ;; Common seventh chords
	 (((0 . 0) (2 . -1) (4 . -1) (6 . -2)) . (rows (super "o") "7"))
	 (((0 . 0) (2 . 0) (4 . 0) (6 . 0)) . ("maj7"))
	 (((0 . 0) (2 . -1) (4 . 0) (6 . -1)) . ("m7"))
	 (((0 . 0) (2 . 0) (4 . 0) (6 . -1)) . ("7"))
	 (((0 . 0) (2 . -1) (4 . 0) (6 . 0)) . ("m(maj7)"))
	 ;jazz: the delta, see jazz-chords.ly
	 ;;(((0 . 0) (2 . -1) (4 . -1) (6 . -2)) .  (super ((font-family . math) "N"))
	 ;; ugh, kludge slashed o
	 (((0 . 0) (2 . -1) (4 . -1) (6 . -1)) . (rows ((raise . 1) "o") ((kern . -0.85) ((raise . 0.57) ((font-relative-size . -3) "/"))) "7")) ; slashed o
	 (((0 . 0) (2 . 0) (4 . 1) (6 . -1)) . ("aug7"))
	 (((0 . 0) (2 . 0) (4 . -1) (6 . 0)) . (rows "maj7" ((font-relative-size . -2) ((raise . 0.2) (music (named "accidentals--1")))) "5"))
	 (((0 . 0) (2 . 0) (4 . -1) (6 . -1)) . (rows "7" ((font-relative-size . -2) ((raise . 0.2) (music (named "accidentals--1")))) "5"))
	 (((0 . 0) (3 . 0) (4 . 0) (6 . -1)) . ("7sus4"))
	 ;; Common ninth chords
	 (((0 . 0) (2 . 0) (4 . 0) (5 . 0) (1 . 0)) . ("6/9")) ;; we don't want the '/no7'
	 (((0 . 0) (2 . 0) (4 . 0) (5 . 0)) . ("6"))
	 (((0 . 0) (2 . -1) (4 . 0) (5 . 0)) . ("m6"))
	 (((0 . 0) (2 . 0) (4 . 0) (1 . 0)) . ("add9"))
	 (((0 . 0) (2 . 0) (4 . 0) (6 . 0) (1 . 0)) . ("maj9"))
	 (((0 . 0) (2 . 0) (4 . 0) (6 . -1) (1 . 0)) . ("9"))
	 (((0 . 0) (2 . -1) (4 . 0) (6 . -1) (1 . 0)) . ("m9"))

	 )
      chord::names-alist-american))

;; Jazz chords, by Atte Andr'e Jensen <atte@post.com>
;; NBs:	This uses the american list as a base.
;;	Some defs take up more than one line,
;; be carefull when messing with ;'s!!


;; FIXME
;;
;; This is getting out-of hand?  Only exceptional chord names that
;; cannot be generated should be here.
;; Maybe we should have inner-jazz-name and inner-american-name functions;
;; 
;;       
;;
;; DONT use non-ascii characters, even if ``it works'' in Windows

(define chord::names-alist-jazz '())
(set! chord::names-alist-jazz
      (append 
      '(
	;; major chords
	; major sixth chord = 6
 	(((0 . 0) (2 . 0) (4 . 0) (5 . 0)) . (((raise . 0.5) "6")))
	; major seventh chord = triangle
	(((0 . 0) (2 . 0) (4 . 0) (6 . 0)) .  (((raise . 0.5)((font-family . "math") "M"))))
	; major chord add nine = add9
	(((0 . 0) (2 . 0) (4 . 0) (1 . 0)) . (((raise . 0.5) "add9")))
	; major sixth chord with nine = 6/9
	(((0 . 0) (2 . 0) (4 . 0) (5 . 0) (1 . 0)) . (((raise . 0.5) "6/9")))

	;; minor chords
	; minor sixth chord = m6
 	(((0 . 0) (2 . -1) (4 . 0) (5 . 0)) . (rows("m")((raise . 0.5) "6")))
	; minor major seventh chord = m triangle
	(((0 . 0) (2 . -1) (4 . 0) (6 . 0)) . (rows ("m") ((raise . 0.5)((font-family . "math") "M"))))
	; minor seventh chord = m7
	(((0 . 0) (2 . -1) (4 . 0) (6 . -1)) . (rows("m")((raise . 0.5) "7")))
	; minor sixth nine chord = m6/9
	(((0 . 0) (2 . -1) (4 . 0) (5 . 0) (1 . 0)) . (rows("m")((raise . 0.5) "6/9")))
	; minor with added nine chord = madd9
	(((0 . 0) (2 . -1) (4 . 0) (1 . 0)) . (rows("m")((raise . 0.5) "add9")))
	; minor ninth chord = m9
	(((0 . 0) (2 . -1) (4 . 0) (6 . -1) (1 . 0)) . (rows("m")((raise . 0.5) "9")))

	;; dominant chords
	; dominant seventh = 7
	(((0 . 0) (2 . 0) (4 . 0) (6 . -1)) . (((raise . 0.5) "7")))
	; augmented dominant = +7
	;(((0 . 0) (2 . 0) (4 . +1) (6 . -1)) . (((raise . 0.5) "+7"))) ; +7 with both raised
	(((0 . 0) (2 . 0) (4 . +1) (6 . -1)) . (rows("+")((raise . 0.5) "7"))) ; +7 with 7 raised
	;(((0 . 0) (2 . 0) (4 . +1) (6 . -1)) . (rows((raise . 0.5) "7(")
	;	((raise . 0.3)(music (named ("accidentals-1"))))
	;	((raise . 0.5) "5)"))); 7(#5)
	; dominant flat 5 = 7(b5)
	(((0 . 0) (2 . 0) (4 . -1) (6 . -1)) . (rows((raise . 0.5) "7(")
		((raise . 0.3)(music (named ("accidentals--1"))))
		((raise . 0.5) "5)")))
	; dominant 9 = 7(9)
	(((0 . 0) (2 . 0) (4 . 0) (6 . -1) (1 . 0)) . (((raise . 0.8)"7(9)")))
	; dominant flat 9 = 7(b9)
	(((0 . 0) (2 . 0) (4 . 0) (6 . -1) (1 . -1)) . (
		((raise . 0.8)"7(")
		((raise . 0.3)(music (named ("accidentals--1"))))
		((raise . 0.8)"9)")))
	; dominant sharp 9 = 7(#9)
	(((0 . 0) (2 . 0) (4 . 0) (6 . -1) (1 . +1)) . (
		((raise . 0.8)"7(")
		((raise . 0.3)(music (named ("accidentals-1"))))
		((raise . 0.8)"9)")))
	; dominant 13 = 7(13)
	(((0 . 0) (2 . 0) (4 . 0) (6 . -1) (5 . 0)) . (((raise . 0.8)"7(13)")))
	; dominant flat 13 = 7(b13)
	(((0 . 0) (2 . 0) (4 . 0) (6 . -1) (5 . -1)) . (
		((raise . 0.8)"7(")
		((raise . 0.3)(music (named ("accidentals--1"))))
		((raise . 0.8)"13)")))
	; dominant 9, 13 = 7(9,13)
	(((0 . 0) (2 . 0) (4 . 0) (6 . -1) (1 . 0) (5 . 0)) . (((raise . 0.8)"7(9, 13)")))
	; dominant flat 9, 13 = 7(b9,13)
	(((0 . 0) (2 . 0) (4 . 0) (6 . -1) (1 . -1) (5 . 0)) . (
		((raise . 0.8)"7(")
		((raise . 0.3)(music (named ("accidentals--1"))))
		((raise . 0.8)"9, 13)")))
	; dominant sharp 9, 13 = 7(#9,13)
	(((0 . 0) (2 . 0) (4 . 0) (6 . -1) (1 . +1) (5 . 0)) . (
		((raise . 0.8)"7(")
		((raise . 0.3)(music (named ("accidentals-1"))))
		((raise . 0.8)"9, 13)")))
	; dominant 9, flat 13 = 7(9,b13)
	(((0 . 0) (2 . 0) (4 . 0) (6 . -1) (1 . 0) (5 . -1)) . (
		((raise . 0.8)"7(9, ")
		((raise . 0.3)(music (named ("accidentals--1"))))
		((raise . 0.8)"13)")))
	; dominant flat 9, flat 13 = 7(b9,b13)
	(((0 . 0) (2 . 0) (4 . 0) (6 . -1) (1 . -1) (5 . -1)) . (
		((raise . 0.8)"7(")
		((raise . 0.3)(music (named ("accidentals--1"))))
		((raise . 0.8)"9, ")
		((raise . 0.3)(music (named ("accidentals--1"))))
		((raise . 0.8)"13)")))
	; dominant sharp 9, flat 13 = 7(#9,b13)
	(((0 . 0) (2 . 0) (4 . 0) (6 . -1) (1 . +1) (5 . -1)) . (
		((raise . 0.8)"7(")
		((raise . 0.3)(music (named ("accidentals-1"))))
		((raise . 0.8)"9, ")
		((raise . 0.3)(music (named ("accidentals--1"))))
		((raise . 0.8)"13)")))

	;; diminished chord(s)
	; diminished seventh chord =  o


	;; DONT use non-ascii characters, even if ``it works'' in Windows
	
	;;(((0 . 0) (2 . -1) (4 . -1) (6 . -2)) . (((raise . 0.8)"o"))); works, but "o" is a little big
	(((0 . 0) (2 . -1) (4 . -1) (6 . -2)) . ((raise . 0.8) (size . -2) ("o")))

	;; half diminshed chords
	; half diminished seventh chord = slashed o
	(((0 . 0) (2 . -1) (4 . -1) (6 . -1)) . (((raise . 0.8)"/o")))
	; half diminished seventh chord  with major 9 = slashed o cancelation 9
	(((0 . 0) (2 . -1) (4 . -1) (6 . -1) (1 . 0)) . (
		((raise . 0.8)"/o(")
		((raise . 0.3)(music (named ("accidentals-0"))))
		((raise . 0.8)"9)"))); 

;; Missing jazz chord definitions go here (note new syntax: see american for hints)

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
      (list
       (append '(music)
	       (list
		(append '(named)
			(list
			  (append '((font-relative-size . -2))
				(list (append '((raise . 0.6))
				  (list
				   (string-append "accidentals-" 
						  (number->string (caddr pitch)))))))))))))))

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
  (apply append
	 '(rows)
	 (pitch->text-banter tonic)
	 (if user-name user-name '())
	 ;; why does list->string not work, format seems only hope...
	 (if (and (string-match "super" (format "~s" user-name))
		  (or (pair? additions)
		      (pair? subtractions)))
	     '((super "/"))
	     '())
	 (let loop ((from additions) (to '()))
	   (if (pair? from)
	       (let ((p (car from)))
		 (loop (cdr from) 
		       (append to
			       (cons
				(list 'super (step->text-banter p))
				(if (or (pair? (cdr from))
					(pair? subtractions))
				    '((super "/"))
				    '())))))
	       to))
	 (let loop ((from subtractions) (to '()))
	   (if (pair? from)
		 (let ((p (car from)))
		   (loop (cdr from) 
			 (append to
				 (cons '(super "no")
				       (cons
					(list 'super (step->text-banter p))
					(if (pair? (cdr from))
					    '((super "/"))
					    '())))))) ; nesting?
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

;; Jazz style--basically similar to american with minor changes
(define (chord::name-jazz tonic user-name pitches base-and-inversion)
  (let ((additions (chord::additions pitches))
	(subtractions #f))
    (chord::inner-name-banter tonic user-name additions subtractions base-and-inversion)))

(define (new-to-old-pitch p)
  (if (pitch? p)
      (list (pitch-octave p) (pitch-notename p) (pitch-alteration p))
      #f
  ))



;; C++ entry point
;; 
;; Check for each subset of chord, full chord first, if there's a
;; user-override.  Split the chord into user-overridden and to-be-done
;; parts, complete the missing user-override matched part with normal
;; chord to be name-calculated.
;;
;; CHORD: (pitches (base . inversion))
(define (default-chord-name-function style chord)
  (let* ((pitches (map new-to-old-pitch (car chord)))
	 (modifiers (cdr chord))
	 (base-and-inversion (if (pair? modifiers)
				 (cons (new-to-old-pitch (car modifiers))
				       (new-to-old-pitch (cdr modifiers)))
				 '(() . ())))
	 (diff (pitch::diff '(0 0 0) (car pitches)))
	 (name-func 
	  (ly-eval (string->symbol (string-append "chord::name-" style))))
	 (names-alist 
	  (ly-eval (string->symbol (string-append "chord::names-alist-" style)))))
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


