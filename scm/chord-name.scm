;;;
;;; chord-name.scm -- Compile chord name
;;;
;;; source file of the GNU LilyPond music typesetter
;;; 
;;; (c) 2000--2002 Jan Nieuwenhuizen <janneke@gnu.org>
;;;


(use-modules
   (ice-9 debug)
   (ice-9 format)
   (ice-9 regex)
   (ice-9 string-fun)
   )


(define (write-me x)
  "Write and return X. For debugging purposes. "
  (write x) (newline) x)

;(define (dbg x) (write-me x))
(define (dbg x) x)

;;(define (write-me x) (write x) (newline) x)
;;(define (write-me-2 x y) (write "FOO") (write x) (write y) (newline) y)


"
TODO:

  * Use lilypond Pitch objects -- SCM pitch objects lead to
    duplication. LilyPond pitch objects force meaningful names
    (i.e. (ly:pitch-octave PITCH) )

  * Pitches are musical objects. The pitches -> markup step should
happen earlier (during interpreting), brew-molecule () should only
dump reinterpret the markup as a molecule.

  *  chord:: prefix is a poor-man's namespace device.
  We have a module system to prevent export to userland.
  Nested functions can take care of many other clashes. --hwn.

   * easier tweakability:

    - split chord::names-alists up into logical bits,
      such as chord::exceptions-delta, exceptions-oslash
    - iso just the 'style parameter, use a list, eg:
      \property ChordNames.ChordName \set
        #'style = #'(jazz delta oslash german-tonic german-Bb)

 * clean split/merge of bass/banter/american stuff.
   GET RID OF code duplication.

 * fix FIXMEs

 * doc strings

"

;; " hey Emacs: string has ended


;; pitch = (octave notename alteration)
;;
;; note = (notename . alteration)
;;
;; markup = markup text -- see font.scm and input/test/markup.ly


(define-public chord::exception-alist-banter
       `(
	; C iso C.no3.no5
	(((0 . 0)) . ,empty-markup)
	; C iso C.no5
	(((0 . 0) (2 . 0)) . ,empty-markup)
	; Cm iso Cm.no5
	(((0 . 0) (2 . -1)) . ,(make-simple-markup "m"))
	; C2 iso C2.no3
	(((0 . 0) (1 . 0) (4 . 0))
	 . ,(make-normal-size-super-markup (make-simple-markup "2 ")))
	; C4 iso C4.no3
	(((0 . 0) (3 . 0) (4 . 0))
	 . ,(make-normal-size-super-markup (make-simple-markup "4 ")))
	;; Cdim iso Cm5-
	(((0 . 0) (2 . -1) (4 . -1)) . ,(make-simple-markup "dim"))
	; URG: Simply C:m5-/maj7 iso Cdim maj7
	(((0 . 0) (2 . -1) (4 . -1) (6 . 0))
	 . ,(make-line-markup
	     (list
	      (make-simple-markup "m")
	      (make-normal-size-super-markup (make-simple-markup "5-/maj7 ")))))
	; URG: Simply C:m5-/7 iso Cdim7
	(((0 . 0) (2 . -1) (4 . -1) (6 . -1))
	 . ,(make-line-markup
	     (list
	      (make-simple-markup "m")
	      (make-normal-size-super-markup (make-simple-markup "5-/7 ")))))
	; Co iso C:m5-/7-
        (((0 . 0) (2 . -1) (4 . -1) (6 . -2))
	 . ,(make-super-markup (make-simple-markup "o")))
	; Cdim9
	(((0 . 0) (2 . -1) (4 . -1) (6 . -2) (1 . -1))
	 . ,(make-line-markup
	     (list (make-simple-markup "dim")
		   (make-normal-size-super-markup (make-simple-markup "9 ")))))
	(((0 . 0) (2 . -1) (4 . -1) (6 . -2) (1 . -1) (3 . -1))
	 . ,(make-line-markup
	     (list (make-simple-markup "dim")
				(make-normal-size-super-markup
				 (make-simple-markup "11 ")))))
	
	))

; pitch->note-name: drops octave
(define (pitch->note-name pitch)
  (cons (cadr pitch) (caddr pitch)))

(define (accidental->markup acc)
  "ACC is an int, return a markup making an accidental."
  (if (= acc 0)
      (make-line-markup (list empty-markup))
      (make-smaller-markup (make-musicglyph-markup
			    (string-append "accidentals-"
					   (number->string acc))))))

; unused.
(define (accidental->markupp acc pos)

  (if (= acc 0)
      empty-markup
      (let ((acc-markup (make-musicglyph-markup
			 (string-append "accidentals-"
					(number->string acc)))))
	
	(if (equal? pos 'columns)
	    (make-line-markup (list (make-smaller-markup acc-markup)))
	  (if (equal? pos 'super)
	      (make-line-markup (list (make-super-markup acc-markup)))
	      ;; not 'super or 'columns must be 'sub...
	      (make-line-markup (list (make-sub-markup acc-markup))))))))


; unused.

;; FIXME: possibly to be used for american/jazz style
;; However, only pos == columns is used, which seems to do
;; exactly what accidental->markup does...
(define (amy-accidental->text acc) (accidental->textp acc 'columns))


;; These not used
;;;(define (accidental->text-super acc) (accidental->textp acc 'simple-super))
;;(define (accidental->text-super acc) (accidental->textp acc 'super))
;;(define (accidental->text-sub acc) (accidental->textp acc 'sub))


;;
;; TODO: invent sensible way to make note name tweaking possible?
;;
(define (pitch->markup pitch)
  (make-line-markup
   (list
    (make-simple-markup
     (vector-ref #("C" "D" "E" "F" "G" "A" "B")  (cadr pitch)))
    (make-normal-size-super-markup
     (accidental->markup (caddr pitch))))))
  
;;; Hooks to override chord names and note names, 
;;; see input/tricks/german-chords.ly

(define pitch->markup-banter pitch->markup)

;; We need also steps, to allow for Cc name override,
;; see input/test/Cc-chords.ly
(define (pitch->chord-name-markup-banter pitch steps)
  (pitch->markup-banter pitch))

(define pitch->note-name-markup-banter pitch->markup-banter)

(define (step->markup pitch)
  (string-append
   (number->string (+ (cadr pitch) (if (= (car pitch) 0) 1 8)))
   (case (caddr pitch)
      ((-2) "--")
      ((-1) "-")
      ((0) "")
      ((1) "+")
      ((2) "++"))))
  
(define (step->markup-banter pitch)
  (make-simple-markup
   (if (= (cadr pitch) 6)
       (case (caddr pitch)
	 ((-2)  "7-")
	 ((-1) "7")
	 ((0)  "maj7")
	 ((1)  "7+")
	 ((2)  "7+"))
       (step->markup pitch))))

(define (step->markup-previously-alternate-jazz pitch)
  (make-line-markup
   (list
    (accidental->markup (caddr pitch))
    (make-simple-markup
     (number->string (+ (cadr pitch) (if (= (car pitch) 0) 1 8)))))))

(define (step->markup-previously-jazz pitch)
  (if (= (cadr pitch) 6)
      (case (caddr pitch)
	;; sharp 7 only included for completeness?
	((-2) (make-line-markup
	       (list
		(accidental->markup  -1)
		(make-simple-markup "7"))))
	((-1) (make-simple-markup "7"))
	((0) (make-simple-markup "maj7"))
	;;((0) (make-line-markup
	;;      (list (make-simple-markup "maj7"))))
	((1) (make-line-markup
	      (list
	       (accidental->markup 1) (make-simple-markup "7"))))
	((2) (make-line-markup
	      (list (accidental->markup 1)
		    (make-simple-markup "7")))))
      (step->markup-previously-alternate-jazz pitch)))


(define pitch::semitone-vec #(0 2 4 5 7 9 11))

(define (pitch::semitone pitch)
  (+ (* (car pitch) 12) 
     (vector-ref pitch::semitone-vec (modulo (cadr pitch) 7)) 
     (caddr pitch)))

(define (pitch::< l r)
  (< (pitch::semitone l) (pitch::semitone r)))
  
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


; what's this? 
(define chord::minor-major-vec #(0 -1 -1 0 -1 -1 0))

;; FIXME: unLOOP
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

(define (chord::additions steps)
" Return:
   * any even step (2, 4, 6)
   * any uneven step that is chromatically altered,
     (where 7-- == -1, 7- == 0, 7 == +1)
   * highest step

?and jazz needs also:

   * TODO: any uneven step that's lower than an uneven step which is
     chromatically altered
  "
  (let ((evens (filter-list (lambda (x) (!= 0 (modulo (cadr x) 2))) steps))
	(altered-unevens
	 (filter-list (lambda (x)
			(let ((n (cadr x)) (a (caddr x)))
			  (or (and (= 6 n) (!= -1 a))
			      (and (!= 6 n)
				   (= 0 (modulo n 2))
				   (!= 0 a)))))
		      steps))
	(highest (let ((h (car (last-pair steps))))
		   (if (and (not (null? h))
			    (or (> 4 (cadr h))
				(!= 0 (caddr h))))
		       (list (list h))
		       '()))))
    ;; Hmm, what if we have a step twice, can we ignore that?
    (uniq-list (sort (apply append evens altered-unevens highest)
		     pitch::<))))
	
     
;; FIXME: unLOOP, see ::additions
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

(define (chord::additions->markup-banter additions subtractions)
  (if (pair? additions)
      (make-line-markup
       (list
	(let ((step (step->markup-banter (car additions))))
	  (if (or (pair? (cdr additions))
		  (pair? subtractions))
	      (make-line-markup
	       (list step (make-simple-markup "/")))
	      step))
	(chord::additions->markup-banter (cdr additions) subtractions)))
      empty-markup))

(define (chord::subtractions->markup-banter subtractions)
  (if (pair? subtractions)
      (make-line-markup
       (list
	(make-simple-markup "no")
	(let ((step (step->markup-previously-jazz
		     (car subtractions))))
	  (if (pair? (cdr subtractions))
	      (make-line-markup
	       (list step (make-simple-markup "/")))
	      step))
	(chord::subtractions->markup-banter (cdr subtractions))))
      empty-markup))

(define (chord::bass-and-inversion->markup-banter bass-and-inversion)
  (if (and (pair? bass-and-inversion)
	   (or (car bass-and-inversion)
	       (cdr bass-and-inversion)))
      (make-line-markup
       (list
	(make-simple-markup "/")
	(pitch->note-name-markup-banter	
	 (if (car bass-and-inversion)
	     (car bass-and-inversion)
	     (cdr bass-and-inversion)))))
      empty-markup))

;; FIXME: merge this function with inner-name-jazz, -american
;;        iso using chord::bass-and-inversion->markup-banter,
;;        See: chord::exceptions-lookup
(define (chord::inner-name-banter tonic exception-part additions subtractions
				  bass-and-inversion steps)
  "
        
 Banter style
 Combine tonic, exception-part of chord name,
 additions, subtractions and bass or inversion into chord name

"
  (let* ((tonic-markup (pitch->chord-name-markup-banter tonic steps))
	 (except-markup (if exception-part exception-part empty-markup))
	 ;; UGR.  How do we know if we should add a separator or not?
	 ;; maybe just add extra column to exception list?
	 (sep-markup (if (and exception-part
			      (let ((s (format "~s" except-markup)))
				(and
				 (string-match "super" s)
				 ;; ugh ugh
				 ;; python: `except_markup`[-5:] != '"o"))'
				 (not (equal?
				       "\"o\"))"
				       (substring s
						  (- (string-length s) 5))))))
			      (or (pair? additions)
				  (pair? subtractions)))
			 (make-super-markup (make-simple-markup "/"))
			 empty-markup))
	 (adds-markup (chord::additions->markup-banter additions subtractions))
	 (subs-markup (chord::subtractions->markup-banter subtractions))
	 (b+i-markup (chord::bass-and-inversion->markup-banter
		      bass-and-inversion)))
    
    (make-line-markup
     (list
      tonic-markup
      except-markup
      sep-markup
      (make-normal-size-super-markup
       (make-line-markup (list adds-markup subs-markup)))
      b+i-markup))))

(define (c++-pitch->scm p)
  (if (ly:pitch? p)
      (list (ly:pitch-octave p) (ly:pitch-notename p) (ly:pitch-alteration p))
      #f))

(define (chord::name-banter tonic exception-part unmatched-steps
			    bass-and-inversion steps)
  (let ((additions (chord::additions unmatched-steps))
	(subtractions (chord::subtractions unmatched-steps)))
    
    (chord::inner-name-banter tonic exception-part additions subtractions
			      bass-and-inversion steps)))


;; see above.
(define (chord::exceptions-lookup exceptions steps)
  "
   return (MATCHED-EXCEPTION . BASE-CHORD-WITH-UNMATCHED-STEPS)
   BASE-CHORD-WITH-UNMATCHED-STEPS always includes (tonic 3 5)

"
  ;; this is unintelligible.
  ;;
  (define (chord::exceptions-lookup-helper
	   exception-alist try-steps unmatched-steps exception-part)
    "

 check exception-alist for biggest matching part of try-steps
 return (MATCHED-EXCEPTION . UNMATCHED-STEPS)

"
    (if (pair? try-steps)
	;; FIXME: junk '(0 . 0) from exceptions lists?
	;;        if so: how to handle first '((0 . 0) . #f) entry?
	;;
	;; FIXME: either format exceptions list as real pitches, ie,
	;;        including octave '((0 2 -1) ..), or drop octave
	;;        from rest of calculations, 
	(let ((entry (assoc
		      (map (lambda (x) (pitch->note-name x))
			   (append '((0 0 0)) try-steps))
		      exception-alist)))
	  (if entry
	      (chord::exceptions-lookup-helper
	       #f '() unmatched-steps (cdr entry))
	      (let ((r (reverse try-steps)))
		(chord::exceptions-lookup-helper
		 exception-alist
		 (reverse (cdr r))
		 (cons (car r) unmatched-steps) #f))))
	(cons exception-part unmatched-steps)))

  (let* ((result (chord::exceptions-lookup-helper
		  exceptions
		  steps '() #f))
	   (exception-part (car result))
	   (unmatched-steps (cdr result))
	   (matched-steps (if (= (length unmatched-steps) 0)
			      3
			      (+ 1 (- (length steps)
				      (length unmatched-steps)))))
	   (unmatched-with-1-3-5
	    (append (do ((i matched-steps (- i 1))
			 (base '() (cons `(0 ,(* (- i 1) 2) 0) base)))
			((= i 0) base)
		      ())
		    unmatched-steps)))
    (list exception-part unmatched-with-1-3-5)))



;;; American style
;;;

;; See input/test/american-chords.ly
;;
;; Original Version by James Hammons, <jlhamm@pacificnet.net>
;; Complete rewrite by Amelie Zapf, <amy@loueymoss.com>

;; DONT use non-ascii characters, even if ``it works'' in Windows

;;a white triangle
(define mathm-markup-object
  (make-override-markup '(font-family . math) (make-simple-markup "M")))

;a black triangle
(define mathn-markup-object
  (make-override-markup '(font-family . math) (make-simple-markup "N")))

(define (step->markup-accidental pitch)
  (make-line-markup
   (list
    (case (caddr pitch)
      ((-2) (accidental->markup -2))
      ((-1) (accidental->markup -1))
      ((0) empty-markup)
      ((1) (accidental->markup 1))
      ((2) (accidental->markup 2)))
    (make-simple-markup (number->string (+ (cadr pitch) (if (= (car pitch) 0) 1 8)))))))

(define-public chord::exception-alist-american 
  `(
    (((0 . 0)) . ,empty-markup)
    (((0 . 0) (2 . -1)) . ,(make-simple-markup "m"))
    
    ;; these should probably be normal-size?  --jcn
    ;;(((0 . 0) (4 . 0)) . ,(make-super-markup (make-simple-markup "5 ")))
    ;;(((0 . 0) (1 . 0) (4 . 0)) . ,(make-super-markup (make-simple-markup "2 ")))
    
    (((0 . 0) (4 . 0)) . ,(make-normal-size-super-markup (make-simple-markup "5 ")))
    (((0 . 0) (1 . 0) (4 . 0)) . ,(make-normal-size-super-markup (make-simple-markup "2 ")))
    
    ;;choose your symbol for the fully diminished chord
    (((0 . 0) (2 . -1) (4 . -1) (6 . -2)) . ,(make-simple-markup "dim"))
    ;;(((0 . 0) (2 . -1) (4 . -1) (6 . -2)) . ,(make-line-markup (list empty-markup (make-super-markup (make-simple-markup "o")))))
    ))

(define (step->markup-american pitch)
  (case (cadr pitch)
    ((6) (case (caddr pitch)
	   ((-2) (make-line-markup (list (accidental->markup -1) (make-simple-markup "7"))))
	   ((-1) (make-simple-markup "7"))
	   ((0) (make-simple-markup "maj7"))
	   ((1) (make-line-markup (list (accidental->markup 1) (make-simple-markup "7"))))
	   ((2) (make-line-markup (list (accidental->markup 2) (make-simple-markup "7"))))))
    ((4) (case (caddr pitch)
	   ((-2) (make-line-markup (list (accidental->markup -2) (make-simple-markup "5"))))
	   ;;choose your symbol for the diminished fifth
	   ((-1) (make-simple-markup "-5"))
	   ;;((-1) (make-line-markup (list (accidental->markup -1) (make-simple-markup "5")))))
	   ((0) empty-markup)
	   ;;choose your symbol for the augmented fifth
	   ;;((1) (make-simple-markup "aug"))
	   ;;((1) (make-line-markup (list (accidental->markup 1) (make-simple-markup "5")))))
	   ((1) (make-simple-markup "+5"))
	   ((2) (make-line-markup (list (accidental->markup 2) (make-simple-markup "5"))))))
    (else (if (and (= (car pitch) 0)
		   (= (cadr pitch) 3)
		   (= (caddr pitch) 0))
	      (make-simple-markup "sus4")
	      (step->markup-accidental pitch)))))
  
(define (chord::additions->markup-american additions subtractions)
  (if (pair? additions)
      ;; I don't like all this reasoning here, when we're actually typesetting.
      (if(and(pair? (cdr additions)) ;a further addition left over
	     (or(and(= 0 (caddr(car additions))) ;this addition natural
		    (not(= 6 (cadr(car additions)))))
		(and(= -1 (caddr(car additions)))
		    (= 6 (cadr(car additions)))))
	     (or(and(= 0 (caddr(cadr additions))) ;the following addition natural
		    (not(= 6 (cadr(cadr additions)))))
		(and(= -1 (caddr(cadr additions)))
		    (= 6 (cadr(cadr additions)))))
	     (or(and(= (car(car additions)) (car(cadr additions))) ;both a third apart
		    (= 2 (- (cadr(cadr additions)) (cadr(car additions)))))
		(and(= 1 (- (car(cadr additions)) (car(car additions))))
		    (= 5 (- (cadr(car additions)) (cadr(cadr additions))))))
	     (or(null? subtractions) ;this or clause protects the "adds"
		(and (pair? subtractions)
		     (or (< (car(cadr additions)) (car(car subtractions)))
			 (and(= (car(cadr additions)) (car(car subtractions)))
			     (< (cadr(cadr additions)) (cadr(car subtractions))))))))
	 (chord::additions->markup-american (cdr additions) subtractions)
	 (make-line-markup
	  (list
	   (let ((step (step->markup-american (car additions))))
	     (if (or (pair? (cdr additions))
		     (pair? subtractions))
		 (if (and (pair? (cdr additions))
			  (or(< 3 (- (cadr(cadr additions)) (cadr(car additions))))
			     (and(< 0 (- (car(cadr additions)) (car(car additions))))
				 (> 4 (- (cadr(car additions)) (cadr(cadr additions)))))))
		     (make-line-markup (list step (make-simple-markup " add")))
		     ;; tweak your favorite separator here
		     ;; (make-line-markup (list step (make-simple-markup "/")))
		     (make-line-markup (list step (make-simple-markup " "))))
		 step))
	   (chord::additions->markup-american (cdr additions) subtractions))))
      empty-markup))

(define (chord::inner-name-american tonic exception-part additions subtractions
				  bass-and-inversion steps)
  (let* ((tonic-markup (pitch->chord-name-markup-banter tonic steps))
	 (except-markup (if exception-part exception-part empty-markup))
	 ;; UGR.  How do we know if we should add a separator or not?
	 ;; maybe just add extra column to exception list?
	 (sep-markup (if (and exception-part
			      (let ((s (format "~s" except-markup)))
				(and
				 (string-match "super" s)
				 ;; ugh ugh
				 ;; python: `except_markup`[-7:] != '"o"))'
				 (not (equal?
				       "\"o\"))))"
				       (substring s
						  (- (string-length s) 7))))))
			      (or (pair? additions)
				  (pair? subtractions)))
			 (make-super-markup (make-simple-markup "/"))
			 empty-markup))
	 ;;this list contains all the additions that go "in line"
	 (prefixes
	  (filter-list
	   (lambda (x)
	     (let ((o (car x)) (n (cadr x)) (a (caddr x)))
	       (and (not (and (= 0 o) (= 2 n))) ;gets rid of unwanted thirds
		    ;;change this if you want it differently
		    (not (and (= 0 o) (= 3 n) (= 0 a))) ;sus4
		    (not (and (= 0 o) (= 4 n) (!= 0 a)))))) ;alt5
	   additions))
	 ;;this list contains all the additions that are patched onto the end
	 ;;of the chord symbol, usually sus4 and altered 5ths.
	 (suffixes
	  ;;take out the reverse if it bothers you in a pathological chord
	  (reverse
	   (filter-list
	    (lambda (x)
	      (let ((o (car x)) (n (cadr x)) (a (caddr x)))
		(and(not (and (= 0 o) (= 2 n))) ;gets rid of unwanted thirds
		    ;;change this correspondingly
		    (or(and (= 0 o) (= 3 n) (= 0 a)) ;sus4
		       (and (= 0 o) (= 4 n) (!= 0 a)))))) ;alt5
	    additions)))
	 (relevant-subs (filter-list
			 (lambda (x) ;catches subtractions higher than 5th
			   (let((o (car x)) (n (cadr x)))
			     (or (> o 0)
				 (> n 4))))
			 subtractions))
	 (pref-markup (chord::additions->markup-american prefixes relevant-subs))
	 (suff-markup (chord::additions->markup-american suffixes relevant-subs))
	 (b+i-markup (chord::bass-and-inversion->markup-banter bass-and-inversion)))
    (make-line-markup
     (list
      tonic-markup except-markup sep-markup
      (make-normal-size-super-markup
       (make-line-markup (list pref-markup suff-markup)))
      b+i-markup))))

(define (chord::additions-american steps)
  (let ((evens (filter-list (lambda (x) (!= 0 (modulo (cadr x) 2))) steps))
	;we let all the unevens pass for now, we'll fix that later.
	(unevens
	 (filter-list (lambda (x)
			(let ((n (cadr x)) (a (caddr x)))
			  (or (and (= 6 n) (!= -1 a))
			      (and (< 3 n)
				   (= 0 (modulo n 2))))))
		      steps))
	(highest (let ((h (car (last-pair steps))))
		   (if (and (not (null? h))
			    (or (> 4 (cadr h))
				(!= 0 (caddr h))))
		       (list (list h))
		       '()))))
    (uniq-list (sort (apply append evens unevens highest)
		     pitch::<))))

  ;; American style chordnames use no "no",
  ;; but otherwise very similar to banter for now
  (define-public (chord::name-american tonic exception-part unmatched-steps
  			      bass-and-inversion steps)
  (let ((additions (chord::additions-american unmatched-steps))
	(subtractions (chord::subtractions unmatched-steps)))
    (chord::inner-name-american tonic exception-part additions subtractions
  			      bass-and-inversion steps)))

  ;;; Jazz style
  ;;;
;; Jazz chords, by Atte Andr'e Jensen <atte@post.com>
;; Complete rewrite by Amelie Zapf (amy@loueymoss.com)

;; FIXME: identical to chord::exception-alist-american, apart from commented
;;        dim chord.  should merge.
(define-public chord::exception-alist-jazz 
  `(
    (((0 . 0)) . ,empty-markup)
    (((0 . 0) (2 . -1)) . ,(make-simple-markup "m"))

    ;; these should probably be normal-size?  --jcn
    ;;(((0 . 0) (4 . 0)) . ,(make-super-markup (make-simple-markup "5 ")))
    ;;(((0 . 0) (1 . 0) (4 . 0)) . ,(make-super-markup (make-simple-markup "2 ")))
    
    (((0 . 0) (4 . 0)) . ,(make-normal-size-super-markup (make-simple-markup "5 ")))
    (((0 . 0) (1 . 0) (4 . 0)) . ,(make-normal-size-super-markup (make-simple-markup "2 ")))
    
    ;;choose your symbol for the fully diminished chord
    ;;(((0 . 0) (2 . -1) (4 . -1) (6 . -2)) . ,(make-simple-markup "dim"))
    (((0 . 0) (2 . -1) (4 . -1) (6 . -2)) . ,(make-line-markup (list (make-simple-markup "") (make-super-markup (make-simple-markup "o")))))
    ))

;; FIXME: rather similar to step->markup-american.  should merge.
(define (step->markup-jazz pitch)
  (case (cadr pitch)
    ((6) (case (caddr pitch)
	   ((-2) (make-line-markup (list (accidental->markup -1) (make-simple-markup "7"))))
	   ((-1) (make-simple-markup "7"))
	   ;;Pick your favorite maj7
	   ((0) mathm-markup-object)  ;;a white triangle
	   ;;((0) mathn-markup-object) ;;a black triangle
	   ;;((0) (make-simple-markup "maj7")) ;;good old maj7
	   ((1) (make-line-markup (list (accidental->markup 1) (make-simple-markup "7"))))
	   ((2) (make-line-markup (list (accidental->markup 2) (make-simple-markup "7"))))))
    ((4) (case (caddr pitch)
	   ((-2) (make-line-markup (list (accidental->markup -2) (make-simple-markup "5"))))
	   ;;choose your symbol for the diminished fifth
	   ;;((-1) (make-simple-markup "-5"))
	   ((-1) (make-line-markup (list (accidental->markup -1) (make-simple-markup "5"))))
	   ((0) empty-markup)
	   ;;choose your symbol for the augmented fifth
	   ;;((1) (make-simple-markup "aug"))
	   ((1) (make-line-markup (list (accidental->markup 1) (make-simple-markup "5"))))
	   ;;((1) (make-simple-markup "+5"))
	   ((2) (make-line-markup (list (accidental->markup 2) (make-simple-markup "5"))))))
    (else (if (and (= (car pitch) 0)
		   (= (cadr pitch) 3)
		   (= (caddr pitch) 0))
	      (make-simple-markup "sus4")
	      (step->markup-accidental pitch)))))

;; FIXME: identical to chord::additions->markup-american,
;; except for -jazz / -american suffixes on calls
(define (chord::additions->markup-jazz additions subtractions)
  (if (pair? additions)
      ;; I don't like all this reasoning here, when we're actually typesetting.
      (if(and(pair? (cdr additions)) ;a further addition left over
	     (or(and(= 0 (caddr(car additions))) ;this addition natural
		    (not(= 6 (cadr(car additions)))))
		(and(= -1 (caddr(car additions)))
		    (= 6 (cadr(car additions)))))
	     (or(and(= 0 (caddr(cadr additions))) ;the following addition natural
		    (not(= 6 (cadr(cadr additions)))))
		(and(= -1 (caddr(cadr additions)))
		    (= 6 (cadr(cadr additions)))))
	     (or(and(= (car(car additions)) (car(cadr additions))) ;both a third apart
		    (= 2 (- (cadr(cadr additions)) (cadr(car additions)))))
		(and(= 1 (- (car(cadr additions)) (car(car additions))))
		    (= 5 (- (cadr(car additions)) (cadr(cadr additions))))))
	     (or(null? subtractions) ;this or clause protects the "adds"
		(and (pair? subtractions)
		     (or (< (car(cadr additions)) (car(car subtractions)))
			 (and(= (car(cadr additions)) (car(car subtractions)))
			     (< (cadr(cadr additions)) (cadr(car subtractions))))))))
	 (chord::additions->markup-jazz (cdr additions) subtractions)
	 (make-line-markup
	  (list
	   (let ((step (step->markup-jazz (car additions))))
	     (if (or (pair? (cdr additions))
		     (pair? subtractions))
		 (if (and (pair? (cdr additions))
			  (or(< 3 (- (cadr(cadr additions)) (cadr(car additions))))
			     (and(< 0 (- (car(cadr additions)) (car(car additions))))
				 (> 4 (- (cadr(car additions)) (cadr(cadr additions)))))))
		     (make-line-markup (list step (make-simple-markup " add")))
		     ;; tweak your favorite separator here
		     ;; (make-line-markup (list step (make-simple-markup "/")))
		     (make-line-markup (list step (make-simple-markup " "))))
		 step))
	   (chord::additions->markup-jazz (cdr additions) subtractions))))
      empty-markup))

;; FIXME: identical to chord::additions->markup-american.
;; except for -jazz / -american suffixes on calls
(define (chord::inner-name-jazz tonic exception-part additions subtractions
				bass-and-inversion steps)
  (let* ((tonic-markup (pitch->chord-name-markup-banter tonic steps))
	 (except-markup (if exception-part exception-part empty-markup))
	 ;; UGR.  How do we know if we should add a separator or not?
	 ;; maybe just add extra column to exception list?
	 (sep-markup (if (and exception-part
			      (let ((s (format "~s" except-markup)))
				(and
				 (string-match "super" s)
				 ;; ugh ugh
				 ;; python: `except_markup`[-7:] != '"o"))'
				 (not (equal?
				       "\"o\"))))"
				       (substring s
						  (- (string-length s) 7))))))
			      (or (pair? additions)
				  (pair? subtractions)))
			 (make-super-markup (make-simple-markup "/"))
			 empty-markup))
	 ;;this list contains all the additions that go "in line"
	 (prefixes
	  (filter-list
	   (lambda (x)
	     (let ((o (car x)) (n (cadr x)) (a (caddr x)))
	       (and (not (and (= 0 o) (= 2 n))) ;gets rid of unwanted thirds
		    ;;change this if you want it differently
		    (not (and (= 0 o) (= 3 n) (= 0 a))) ;sus4
		    (not (and (= 0 o) (= 4 n) (!= 0 a)))))) ;alt5
	   additions))
	 ;;this list contains all the additions that are patched onto the end
	 ;;of the chord symbol, usually sus4 and altered 5ths.
	 (suffixes
	  ;;take out the reverse if it bothers you in a pathological chord
	  (reverse
	   (filter-list
	    (lambda (x)
	      (let ((o (car x)) (n (cadr x)) (a (caddr x)))
		(and(not (and (= 0 o) (= 2 n))) ;gets rid of unwanted thirds
		    ;;change this correspondingly
		    (or(and (= 0 o) (= 3 n) (= 0 a)) ;sus4
		       (and (= 0 o) (= 4 n) (!= 0 a)))))) ;alt5
	    additions)))
	 (relevant-subs (filter-list
			 (lambda (x) ;catches subtractions higher than 5th
			   (let((o (car x)) (n (cadr x)))
			     (or (> o 0)
				 (> n 4))))
			 subtractions))
	 (pref-markup (chord::additions->markup-jazz prefixes relevant-subs))
	 (suff-markup (chord::additions->markup-jazz suffixes relevant-subs))
	 (b+i-markup (chord::bass-and-inversion->markup-banter bass-and-inversion)))
    (make-line-markup
     (list
      tonic-markup except-markup sep-markup
      (make-normal-size-super-markup
       (make-line-markup (list pref-markup suff-markup)))
      b+i-markup))))

(define (chord::name-jazz tonic exception-part unmatched-steps
			  bass-and-inversion steps)
  (let ((additions (chord::additions-american unmatched-steps))
	(subtractions (chord::subtractions unmatched-steps)))
    (chord::inner-name-jazz tonic exception-part additions subtractions
			    bass-and-inversion steps)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-public (new-chord->markup func ly-pitches bass inversion exceptions)
  "Entry point for New_chord_name_engraver. See chord-name.scm for the
signature of FUNC.  LY-PITCHES, BASS and INVERSION are lily
pitches. EXCEPTIONS is an alist (see scm file).
 "
  
  (let* ((pitches (map c++-pitch->scm ly-pitches))
	 (bass-and-inversion 
	  (cons (c++-pitch->scm bass)
		(c++-pitch->scm inversion)))
	 (diff (pitch::diff '(0 0 0) (car pitches)))
	 (steps (if (cdr pitches) (map (lambda (x)
					 (pitch::transpose x diff))
				       (cdr pitches))
		    '()))
	 (lookup (dbg (chord::exceptions-lookup exceptions steps)))
	 (exception-part (dbg (car lookup)))
	 (unmatched-steps (cadr lookup))
	 (tonic (car pitches))	 
	 )

      (func tonic exception-part unmatched-steps bass-and-inversion steps)
      ))
    
(define-public (chord->markup-jazz . args)
  (apply new-chord->markup (cons chord::name-jazz args))
  )

(define-public (chord->markup-american . args)
  (apply new-chord->markup (cons chord::name-american args))
  )

(define-public (chord->markup-banter . args)
  (apply new-chord->markup (cons chord::name-banter args))
  )

(define-public (new-chord-name-brew-molecule grob)
  (let*
      (
       (ws (ly:get-grob-property grob 'word-space))
       (markup (ly:get-grob-property grob 'text))
       (molecule (interpret-markup grob
				   (cons '((word-space . 0.0))
					 (Font_interface::get_property_alist_chain grob))
				   markup))
       )

    ;;
    ;; chord names aren't in staffs, so WS is in global staff space.
    (if (number? ws)
	(ly:combine-molecule-at-edge
	 molecule
	 X RIGHT (ly:make-molecule "" (cons 0 ws) '(-1 . 1) )
	 0.0)
	molecule)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (set-chord-name-style sym)
  "Return music expressions that set the chord naming style. For
inline use in .ly file"
  
  (define (chord-name-style-setter function exceptions)
    (context-spec-music
     (make-sequential-music 
      (list (make-property-set 'chordNameFunction function)
	    (make-property-set 'chordNameExceptions exceptions)))
     "ChordNames"
     )
    )

  (ly:export
   (case sym
     ((jazz)
      (chord-name-style-setter chord->markup-jazz
			       chord::exception-alist-jazz))
     ((banter)
      (chord-name-style-setter chord->markup-banter
			       chord::exception-alist-banter))
     ((american)
      (chord-name-style-setter chord->markup-american
			       chord::exception-alist-american))

     ((ignatzek)
      (chord-name-style-setter ignatzek-chord-names
			       '()))
     ((double-plus-new-banter)
      (chord-name-style-setter double-plus-new-chord->markup-banter
       chord::exception-alist-banter))
     
     ((double-plus-new-jazz)
      (chord-name-style-setter double-plus-new-chord->markup-jazz
       chord::exception-alist-jazz))
     )))

;; can't put this in double-plus-new-chord-name.scm, because we can't
;; ly:load that very easily.
(define-public (set-double-plus-new-chord-name-style style options)
  "Return music expressions that set the chord naming style. For
inline use in .ly file"
  
  (define (chord-name-style-setter function)
    (context-spec-music
     (make-sequential-music 
      (list (make-property-set 'chordNameFunction function)

	    ;; urg , misuse of chordNameExceptions function.
	    (make-property-set 'chordNameExceptions options)))
     "ChordNames"))

  (ly:export
   (case style
     ((banter)
      (chord-name-style-setter double-plus-new-chord->markup-banter))
     
     ((jazz)
      (chord-name-style-setter double-plus-new-chord->markup-jazz)))))

