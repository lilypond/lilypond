;;;
;;; chord-name.scm -- Compile chord name
;;;
;;; source file of the GNU LilyPond music typesetter
;;; 
;;; (c) 2000--2001 Jan Nieuwenhuizen <janneke@gnu.org>
;;;


(use-modules
   (ice-9 debug)
   (ice-9 format)
   (ice-9 regex)
   (ice-9 string-fun)
   )

;; pitch = (octave notename accidental)
;;
;; note = (notename . accidental)
;;
;; text = scm markup text -- see font.scm and input/test/markup.ly


;; TODO
;;
;; * easier tweakability:
;;    - split chord::names-alists up into logical bits,
;;      such as chord::exceptions-delta, exceptions-oslash
;;    - iso just the 'style parameter, use a list, eg:
;;      \property ChordNames.ChordName \set
;;        #'style = #'(jazz delta oslash german-tonic german-Bb)
;;
;; * fix FIXMEs
;;
;; * clean split/merge of bass/banter/american stuff
;;
;; * doc strings

; this sets the basic Banter names
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
	(((0 . 0) (1 . 0) (4 . 0)) . ("" (super "2") " "))
	; C4 iso C4.no3
	(((0 . 0) (3 . 0) (4 . 0)) . ("" (super "4") " " ))
	;; Cdim iso Cm5-
	(((0 . 0) (2 . -1) (4 . -1)) . ("dim"))
	; URG: Simply C:m5-/maj7 iso Cdim maj7
	(((0 . 0) (2 . -1) (4 . -1) (6 . 0)) . ("m" (super "5-/maj7" " ")))
	; URG: Simply C:m5-/7 iso Cdim7
	(((0 . 0) (2 . -1) (4 . -1) (6 . -1)) . ("m" (super "5-/7" " ")))
	; Co iso C:m5-/7-
        (((0 . 0) (2 . -1) (4 . -1) (6 . -2)) . ("" (super "o") " "))
	; Cdim9
	(((0 . 0) (2 . -1) (4 . -1) (6 . -2) (1 . -1)) . ("dim" (super "9") " "))
	(((0 . 0) (2 . -1) (4 . -1) (6 . -2) (1 . -1) (3 . -1)) . ("dim" (super "11") " "))
	)
      chord::names-alist-banter))

;;;;;;;;;;
(define simple-super
;; duh, no docstrings for 
;;  "No real superscript, just raised and small"
  '((raise . 1) (font-relative-size . -2)))

;(define (accidental->textp acc pos)
;  (if (= acc 0)
;      '()
;      ;(list '(music (font-relative-size . -2))
;      (list '(music (raise . 1) (font-relative-size . -2))
;		   (list pos (string-append "accidentals-" (number->string acc))))))

(define (accidental->textp acc pos)
  (if (= acc 0)
      '()
      (if (equal? pos 'columns)
        (list '(music (font-relative-size . -1))
                   (list (string-append "accidentals-" (number->string acc))))
        (if (equal? pos 'super)
          (list '(music (raise . 2) (font-relative-size . -1))
                   (list (string-append "accidentals-" (number->string acc))))
          (list '(music (raise . -1) (font-relative-size . -1))
                   (list (string-append "accidentals-" (number->string acc))))))))

(define (accidental->text acc) (accidental->textp acc 'columns))
;(define (accidental->text-super acc) (accidental->textp acc 'simple-super))
(define (accidental->text-super acc) (accidental->textp acc 'super))
(define (accidental->text-sub acc) (accidental->textp acc 'sub))

; pitch->note-name: drops octave
(define (pitch->note-name pitch)
  (cons (cadr pitch) (caddr pitch)))

(define (pitch->text pitch)
  (text-append
   (list
    '(font-relative-size . 2)
    (make-string 1 (integer->char (+ (modulo (+ (cadr pitch) 2) 7) 65))))
   (accidental->text-super (caddr pitch))))


;;; Hooks to override chord names and note names, 
;;; see input/tricks/german-chords.ly

(define (pitch->text-banter pitch)
  (pitch->text pitch))

;; We need also steps, to allow for Cc name override,
;; see input/test/Cc-chords.ly
(define (pitch->chord-name-text-banter pitch steps)
  (pitch->text-banter pitch))

(define (pitch->note-name-text-banter pitch)
  (pitch->text-banter pitch))

(define (step->text pitch)
  (list (string-append
    (number->string (+ (cadr pitch) (if (= (car pitch) 0) 1 8)))
    (case (caddr pitch)
      ((-2) "--")
      ((-1) "-")
      ((0) "")
      ((1) "+")
      ((2) "++")))))
  
(define (step->text-banter pitch)
  (if (= (cadr pitch) 6)
      (case (caddr pitch)
	((-2) '("7-"))
	((-1) '("7"))
	((0) '("maj7"))
	((1) '("7+"))
	((2) '("7+")))
      (step->text pitch)))

(define pitch::semitone-vec (list->vector '(0 2 4 5 7 9 11)))

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

(define (chord::text? text)
  (not (or (not text) (empty? text) (unspecified? text))))

;; FIXME: remove need for me, use text-append throughout
(define (chord::text-cleanup dirty)
  "
   Recursively remove '() #f, and #<unspecified> from markup text tree.
   This allows us to leave else parts of (if # #) off.
   Otherwise, you'd have to do (if # # '()), and you'd have to
   filter-out the '() anyway.
  "
  (if (pair? dirty)
      (let ((r (car dirty)))
	(if (chord::text? r)
	    (cons (if (pair? r) (chord::text-cleanup r) r)
		  (chord::text-cleanup (cdr dirty)))
	    (chord::text-cleanup (cdr dirty))))
      (if (chord::text? dirty)
	  dirty
	  '())))

(define (text-append l . r)
  (if (not (chord::text? r))
      l
      (if (not (chord::text? l))
	  r
	  (if (empty? (cdr r))
	      (list 'columns l (car r))
	      (text-append (list 'columns l (car r)) (cdr r))))))
	   
(define (chord::step tonic pitch)
 (- (pitch::note-pitch pitch) (pitch::note-pitch tonic)))

;; text: list of word
;; word: string + optional list of property
;; property: align, kern, font (?), size

(define chord::minor-major-vec (list->vector '(0 -1 -1 0 -1 -1 0)))

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
		   (if (and (not (empty? h))
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

(define (chord::additions->text-banter additions subtractions)
  (if (pair? additions)
      (text-append
       (let ((step (step->text-banter (car additions))))
	 (if (or (pair? (cdr additions))
		 (pair? subtractions))
	     (text-append step "/")
	     step))
      (chord::additions->text-banter (cdr additions) subtractions))
  '()))

(define (chord::subtractions->text-banter subtractions)	 
  (if (pair? subtractions)
      (text-append
       '("no")
       (let ((step (step->text-jazz (car subtractions))))
	 (if (pair? (cdr subtractions))
			(text-append step "/")
			step))
       (chord::subtractions->text-banter (cdr subtractions)))
      '()))

(define (chord::bass-and-inversion->text-banter bass-and-inversion)
  (if (and (pair? bass-and-inversion)
	   (or (car bass-and-inversion)
	       (cdr bass-and-inversion)))
      (list "/" (if (car bass-and-inversion)
		    (pitch->note-name-text-banter
		     (car bass-and-inversion))
		    (pitch->note-name-text-banter
		     (cdr bass-and-inversion))))))

;; FIXME: merge this function with inner-name-jazz, -american
;;        iso using chord::bass-and-inversion->text-banter,
;;        call (chord::restyle 'chord::bass-and-inversion->text- style)
;;        See: chord::exceptions-lookup
;;        
;; Banter style
;; Combine tonic, exception-part of chord name,
;; additions, subtractions and bass or inversion into chord name
(define (chord::inner-name-banter tonic exception-part additions subtractions
				  bass-and-inversion steps)
  (let* ((tonic-text (pitch->chord-name-text-banter tonic steps))
	 (except-text exception-part)
	 (sep-text (if (and (string-match "super" (format "~s" except-text))
			    (or (pair? additions)
				(pair? subtractions)))
		       (list simple-super "/")))
	 (adds-text (chord::additions->text-banter additions subtractions))
	 (subs-text (chord::subtractions->text-banter subtractions))
	 (b+i-text (chord::bass-and-inversion->text-banter bass-and-inversion)))

    (text-append
     tonic-text except-text  sep-text
     ;;(list (list simple-super) adds-text subs-text)
     (list (list '((raise . 1) (font-relative-size . -1))) adds-text subs-text)
     b+i-text)))
; I don't want to touch Banter style, but wouldn't that be better? Amy
;     (text-append
;      tonic-text except-text sep-text
;      (text-append
;       (list '((raise . 1) (font-relative-size . -1)) adds-text subs-text)
;       b+i-text))))

(define (c++-pitch->scm p)
  (if (pitch? p)
      (list (pitch-octave p) (pitch-notename p) (pitch-alteration p))
      #f))

(define (chord::name-banter tonic exception-part unmatched-steps
			    bass-and-inversion steps)
   (let ((additions (chord::additions unmatched-steps))
	 (subtractions (chord::subtractions unmatched-steps)))
     (chord::inner-name-banter tonic exception-part additions subtractions
			       bass-and-inversion steps)))


(define (chord::restyle name style)
  (primitive-eval (string->symbol
	    (string-append (symbol->string name)
			   (symbol->string style)))))

;; check exceptions-alist for biggest matching part of try-steps
;; return (MATCHED-EXCEPTION . UNMATCHED-STEPS)
(define (chord::exceptions-lookup-helper
	 exceptions-alist try-steps unmatched-steps exception-part)
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
		    exceptions-alist)))
	(if entry
	    (chord::exceptions-lookup-helper
	     #f '() unmatched-steps (cdr entry))
	    (let ((r (reverse try-steps)))
	      (chord::exceptions-lookup-helper
	       exceptions-alist
	       (reverse (cdr r))
	       (cons (car r) unmatched-steps) #f))))
      (cons exception-part unmatched-steps)))

;; return (MATCHED-EXCEPTION . BASE-CHORD-WITH-UNMATCHED-STEPS)
;; BASE-CHORD-WITH-UNMATCHED-STEPS always includes (tonic 3 5)
(define (chord::exceptions-lookup style steps)
  (let* ((result (chord::exceptions-lookup-helper
		  (chord::restyle 'chord::names-alist- style)
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


(define (chord::name->text style tonic steps bass-and-inversion)
  (let* ((lookup (chord::exceptions-lookup style steps))
	 (exception-part (car lookup))
	 (unmatched-steps (cadr lookup)))
    (chord::text-cleanup
     ((chord::restyle 'chord::name- style)
      tonic exception-part unmatched-steps bass-and-inversion steps))))

;; C++ entry point
;;
;; Check for each subset of chord, full chord first, if there's a
;; user-override.  Split the chord into user-overridden and to-be-done
;; parts, complete the missing user-override matched part with normal
;; chord to be name-calculated.
;;
;; CHORD: (pitches (bass . inversion))
(define-public (default-chord-name-function style chord)
  (let* ((pitches (map c++-pitch->scm (car chord)))
	 (modifiers (cdr chord))
	 (bass-and-inversion (if (pair? modifiers)
				 (cons (c++-pitch->scm (car modifiers))
				       (c++-pitch->scm (cdr modifiers)))
				 '(() . ())))
	 (diff (pitch::diff '(0 0 0) (car pitches)))
	 (steps (if (cdr pitches) (map (lambda (x)
					 (pitch::transpose x diff))
				       (cdr pitches))
		    '())))
;    (display (chord::name->text style (car pitches) steps bass-and-inversion))
    (chord::name->text style (car pitches) steps bass-and-inversion)


    ))

;;;
;;; American style
;;;

;; See input/test/american-chords.ly
;;
;; Original Version by James Hammons, <jlhamm@pacificnet.net>
;; Complete rewrite by Amelie Zapf, <amy@loueymoss.com>

;; DONT use non-ascii characters, even if ``it works'' in Windows

(define chord::names-alist-american '())

(set! chord::names-alist-american
      (append
       '(
	 (((0 . 0)) . #f)
	 (((0 . 0)) . #f)
	 (((0 . 0) (2 . -1)) . ("m"))
         (((0 . 0) (4 . 0)) . '(((raise . 1) (font-relative-size . -1)) "5 "))
	 (((0 . 0) (1 . 0) (4 . 0)) . '(((raise . 1) (font-relative-size . -1)) "2 "))
	 ;choose your symbol for the fully diminished chord
	 (((0 . 0) (2 . -1) (4 . -1) (6 . -2)) . ("dim"))
	 ;(((0 . 0) (2 . -1) (4 . -1) (6 . -2)) . ("" (super "o")))
	 )
       chord::names-alist-american))

(define (step->text-accidental pitch)
  (list (text-append
    (case (caddr pitch)
      ((-2) (accidental->text -2))
      ((-1) (accidental->text -1))
      ((0) "")
      ((1) (accidental->text 1))
      ((2) (accidental->text 2)))
    (number->string (+ (cadr pitch) (if (= (car pitch) 0) 1 8))))))

(define (step->text-american pitch)
  (case (cadr pitch)
    ((6) (case (caddr pitch)
	((-2) (text-append (accidental->text -1) '("7")))
	((-1) '("7"))
	((0) '("maj7"))
	((1) (text-append (accidental->text 1) '("7")))
	((2) (text-append (accidental->text 2) '("7")))))
    ((4) (case (caddr pitch)
	((-2) (text-append (accidental->text -2) '("5")))
	;choose your symbol for the diminished fifth
	((-1) '("-5"))
	;((-1) (text-append (accidental->text -1) '("5")))
	((0) '(""))
	;choose your symbol for the augmented fifth
	;((1) '("aug"))
	;((1) (text-append (accidental->text 1) '("5")))
	((1) '("+5"))
	((2) (text-append (accidental->text 2) '("5")))))
    (else (if (and (= (car pitch) 0)
		   (= (cadr pitch) 3)
		   (= (caddr pitch) 0))
	      '("sus4")
	      (step->text-accidental pitch)))))

(define (chord::additions->text-american additions subtractions)
  (if (pair? additions)
     ; I don't like all this reasoning here, when we're actually typesetting.
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
 	    (or(empty? subtractions) ;this or clause protects the "adds"
 	       (and (pair? subtractions)
 		    (or (< (car(cadr additions)) (car(car subtractions)))
 			(and(= (car(cadr additions)) (car(car subtractions)))
 			    (< (cadr(cadr additions)) (cadr(car subtractions))))))))
         (chord::additions->text-american (cdr additions) subtractions)
      (text-append
       (let ((step (step->text-american (car additions))))
	 (if (or (pair? (cdr additions))
		 (pair? subtractions))
	     (if (and (pair? (cdr additions))
		      (or(< 3 (- (cadr(cadr additions)) (cadr(car additions))))
			 (and(< 0 (- (car(cadr additions)) (car(car additions))))
			     (> 4 (- (cadr(car additions)) (cadr(cadr additions)))))))
		 (text-append step " add")
	     ; tweak your favorite separator here
	     ; (text-append step "/")
	     (text-append step " "))
	     step))
      (chord::additions->text-american (cdr additions) subtractions))
      )
  '()))

(define (chord::inner-name-american tonic exception-part additions subtractions
				  bass-and-inversion steps)
  (let* ((tonic-text (pitch->chord-name-text-banter tonic steps))
	 (except-text exception-part)
	 (sep-text (if (and (string-match "super" (format "~s" except-text))
			    (or (pair? additions)
				(pair? subtractions)))
		       (list simple-super "/")))
	 ;this list contains all the additions that go "in line"
	 	 (prefixes
	  (filter-list (lambda (x)
			 (let ((o (car x)) (n (cadr x)) (a (caddr x)))
			   (and (not (and (= 0 o) (= 2 n))) ;gets rid of unwanted thirds
				;change this if you want it differently
				(not (and (= 0 o) (= 3 n) (= 0 a))) ;sus4
				(not (and (= 0 o) (= 4 n) (!= 0 a)))))) ;alt5
		       additions))
	 ;this list contains all the additions that are patched onto the end
	 ;of the chord symbol, usually sus4 and altered 5ths.
	 (suffixes
	  ;take out the reverse if it bothers you in a pathological chord
	  (reverse (filter-list (lambda (x)
			 (let ((o (car x)) (n (cadr x)) (a (caddr x)))
			   (and(not (and (= 0 o) (= 2 n))) ;gets rid of unwanted thirds
			   ;change this correspondingly
			       (or(and (= 0 o) (= 3 n) (= 0 a)) ;sus4
				  (and (= 0 o) (= 4 n) (!= 0 a)))))) ;alt5
		       additions)))
	 (relevant-subs (filter-list (lambda (x) ;catches subtractions higher than 5th
				       (let((o (car x)) (n (cadr x)))
					 (or (> o 0)
					     (> n 4))))
				     subtractions))
	 (pref-text (chord::additions->text-american prefixes relevant-subs))
	 (suff-text (chord::additions->text-american suffixes relevant-subs))
	 (b+i-text (chord::bass-and-inversion->text-banter bass-and-inversion)))
    (text-append
     tonic-text except-text sep-text
     (text-append
      (list '((raise . 1) (font-relative-size . -1)) pref-text suff-text)
      b+i-text))))

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
		   (if (and (not (empty? h))
			    (or (> 4 (cadr h))
				(!= 0 (caddr h))))
		       (list (list h))
		       '()))))
    (uniq-list (sort (apply append evens unevens highest)
		     pitch::<))))

;; American style chordnames use no "no",
;; but otherwise very similar to banter for now
(define (chord::name-american tonic exception-part unmatched-steps
			      bass-and-inversion steps)
  (let ((additions (chord::additions-american unmatched-steps))
	(subtractions (chord::subtractions unmatched-steps)))
    (chord::inner-name-american tonic exception-part additions subtractions
			      bass-and-inversion steps)))



;;;
;;; Jazz style
;;;
;; Jazz chords, by Atte Andr'e Jensen <atte@post.com>
;; Complete rewrite by Amelie Zapf (amy@loueymoss.com)

(define chord::names-alist-jazz '())

(set! chord::names-alist-jazz
      (append
       '(
	 (((0 . 0)) . #f)
	 (((0 . 0)) . #f)
	 (((0 . 0) (2 . -1)) . ("m"))
         (((0 . 0) (4 . 0)) . '(((raise . 1) (font-relative-size . -1)) "5 "))
	 (((0 . 0) (1 . 0) (4 . 0)) . '(((raise . 1) (font-relative-size . -1)) "2 "))
	 ;choose your symbol for the fully diminished chord
	 ;(((0 . 0) (2 . -1) (4 . -1) (6 . -2)) . ("dim"))
	 (((0 . 0) (2 . -1) (4 . -1) (6 . -2)) . ("" (super "o")))
	 )
       chord::names-alist-jazz))

(define (step->text-jazz pitch)
  (case (cadr pitch)
    ((6) (case (caddr pitch)
	((-2) (text-append (accidental->text -1) '("7")))
	((-1) '("7"))
	;Pick your favorite maj7
	((0) (text-append '((font-family . math) "M")))  ;an open triangle
	;((0) (text-append '((font-family . math) "N"))) ;a filled triangle
	;((0) '("maj7"))                                 ;good old maj7
	((1) (text-append (accidental->text 1) '("7")))
	((2) (text-append (accidental->text 2) '("7")))))
    ((4) (case (caddr pitch)
	((-2) (text-append (accidental->text -2) '("5")))
	;choose your symbol for the diminished fifth
	;((-1) '("-5"))
	((-1) (text-append (accidental->text -1) '("5")))
	((0) '(""))
	;choose your symbol for the augmented fifth
	;((1) '("aug"))
	((1) (text-append (accidental->text 1) '("5")))
	;((1) '("+5"))
	((2) (text-append (accidental->text 2) '("5")))))
    (else (if (and (= (car pitch) 0)
		   (= (cadr pitch) 3)
		   (= (caddr pitch) 0))
	      '("sus4")
	      (step->text-accidental pitch)))))

(define (chord::additions->text-jazz additions subtractions)
  (if (pair? additions)
     ; I don't like all this reasoning here, when we're actually typesetting.
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
 	    (or(empty? subtractions) ;this or clause protects the "adds"
 	       (and (pair? subtractions)
 		    (or (< (car(cadr additions)) (car(car subtractions)))
 			(and(= (car(cadr additions)) (car(car subtractions)))
 			    (< (cadr(cadr additions)) (cadr(car subtractions))))))))
         (chord::additions->text-jazz (cdr additions) subtractions)
      (text-append
       (let ((step (step->text-jazz (car additions))))
	 (if (or (pair? (cdr additions))
		 (pair? subtractions))
	     (if (and (pair? (cdr additions))
		      (or(< 3 (- (cadr(cadr additions)) (cadr(car additions))))
			 (and(< 0 (- (car(cadr additions)) (car(car additions))))
			     (> 4 (- (cadr(car additions)) (cadr(cadr additions)))))))
		 (text-append step " add")
		 ;; tweak your favorite separator here
		 ;; (text-append step "/")
		 (text-append step " "))
	     step))
      (chord::additions->text-jazz (cdr additions) subtractions))
      )
  '()))

(define (chord::inner-name-jazz tonic exception-part additions subtractions
				  bass-and-inversion steps)
  (let* ((tonic-text (pitch->chord-name-text-banter tonic steps))
	 (except-text exception-part)
	 (sep-text (if (and (string-match "super" (format "~s" except-text))
			    (or (pair? additions)
				(pair? subtractions)))
		       (list simple-super "/")))
	 ;this list contains all the additions that go "in line"
	 (prefixes
	  (filter-list (lambda (x)
			 (let ((o (car x)) (n (cadr x)) (a (caddr x)))
			   (and (not (and (= 0 o) (= 2 n))) ;gets rid of unwanted thirds
				;change this if you want it differently
				(not (and (= 0 o) (= 3 n) (= 0 a))) ;sus4
				(not (and (= 0 o) (= 4 n) (!= 0 a)))))) ;alt5
		       additions))
	 ;this list contains all the additions that are patched onto the end
	 ;of the chord symbol, usually sus4 and altered 5ths.
	 (suffixes
	  ;take out the reverse if it bothers you in a pathological chord
	  (reverse (filter-list (lambda (x)
			 (let ((o (car x)) (n (cadr x)) (a (caddr x)))
			   (and(not (and (= 0 o) (= 2 n))) ;gets rid of unwanted thirds
			   ;change this correspondingly
			       (or(and (= 0 o) (= 3 n) (= 0 a)) ;sus4
				  (and (= 0 o) (= 4 n) (!= 0 a)))))) ;alt5
		       additions)))
	 (relevant-subs (filter-list (lambda (x) ;catches subtractions higher than 5th
				       (let((o (car x)) (n (cadr x)))
					 (or (> o 0)
					     (> n 4))))
				     subtractions))
	 (pref-text (chord::additions->text-jazz prefixes relevant-subs))
	 (suff-text (chord::additions->text-jazz suffixes relevant-subs))
	 (b+i-text (chord::bass-and-inversion->text-banter bass-and-inversion)))
    (text-append
     tonic-text except-text sep-text
     (text-append
      (list '((raise . 1) (font-relative-size . -1)) pref-text suff-text)
      b+i-text))))

(define (chord::name-jazz tonic exception-part unmatched-steps
			      bass-and-inversion steps)
  (let ((additions (chord::additions-american unmatched-steps))
	(subtractions (chord::subtractions unmatched-steps)))
    (chord::inner-name-jazz tonic exception-part additions subtractions
			      bass-and-inversion steps)))

