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

(define (accidental->textp acc pos)
  (if (= acc 0)
      '()
      (list '(music (font-relative-size . -2))
		   (list pos (string-append "accidentals-" (number->string acc))))))

(define (accidental->text acc) (accidental->textp acc 'columns))
(define (accidental->text-super acc) (accidental->textp acc 'simple-super))
(define (accidental->text-sub acc) (accidental->textp acc 'sub))

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
     tonic-text except-text " " sep-text
     ;;(list (list simple-super) adds-text subs-text)
     (list (list '((raise . 1) (font-relative-size . -1))) adds-text subs-text)
     b+i-text)))

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
(define (default-chord-name-function style chord)
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
    (chord::name->text style (car pitches) steps bass-and-inversion)))

;;;
;;; American style
;;;


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
;Alternate:	 (((0 . 0) (2 . -1) (4 . -1)) . ("" (super "o")))
	 (((0 . 0) (2 . 0) (4 . 1)) . ("aug"))
;Alternate:	 (((0 . 0) (2 . 0) (4 . 1)) . ("+"))
	 (((0 . 0) (1 . 0) (4 . 0)) . ("2"))
	 ;; Common seventh chords
	 (((0 . 0) (2 . -1) (4 . -1) (6 . -2)) . ("" (super "o") " " "7"))
	 (((0 . 0) (2 . 0) (4 . 0) (6 . 0)) . ("maj7"))
	 ;; urg! should use (0 . 0 2 . -1) -> "m", and add "7" to that!!
	 (((0 . 0) (2 . -1) (4 . 0) (6 . -1)) . ("m7"))
	 (((0 . 0) (2 . 0) (4 . 0) (6 . -1)) . ("7"))
	 (((0 . 0) (2 . -1) (4 . 0) (6 . 0)) . ("m(maj7)"))
	 ;jazz: the delta, see jazz-chords.ly
	 ;;(((0 . 0) (2 . -1) (4 . -1) (6 . -2)) .  (super ((font-family . math) "N"))
	 ;; slashed o
	 (((0 . 0) (2 . -1) (4 . -1) (6 . -1)) . (columns (super (overstrike "o") "/") " " "7"))

	 (((0 . 0) (2 . 0) (4 . 1) (6 . -1)) . ("aug7"))
	 (((0 . 0) (2 . 0) (4 . -1) (6 . 0)) . (columns "maj7" ((font-relative-size . -2) ((raise . 0.2) (music (named "accidentals--1")))) "5"))
	 (((0 . 0) (2 . 0) (4 . -1) (6 . -1)) . (columns "7" ((font-relative-size . -2) ((raise . 0.2) (music (named "accidentals--1")))) "5"))
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


;; American style chordnames use no "no",
;; but otherwise very similar to banter for now
(define (chord::name-american tonic exception-part unmatched-steps
			      bass-and-inversion steps)
  (let ((additions (chord::additions unmatched-steps))
	(subtractions #f))
    (chord::inner-name-banter tonic exception-part additions subtractions
			      bass-and-inversion steps)))



;;; 
;;; Jazz style
;;;



;; Jazz chords, by Atte Andr'e Jensen <atte@post.com>
;; NBs:	This uses the american list as a bass.
;;	Some defs take up more than one line,
;; be carefull when messing with ;'s!!


;; FIXME
;;
;; This is getting out-of hand?  Only exceptional chord names that
;; cannot be generated should be here.
;; Maybe we should have inner-name-jazz and inner-name-american functions;
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
	;; shouldn't this be a filled black triange, like this:  ? --jcn
	;; (((0 . 0) (2 . 0) (4 . 0) (6 . 0)) .  (((raise . 0.5)((font-family . math) "N"))))
	(((0 . 0) (2 . 0) (4 . 0) (6 . 0)) .  (((raise . 0.5)((font-family . math) "M"))))
	; major chord add nine = add9
	(((0 . 0) (2 . 0) (4 . 0) (1 . 0)) . (((raise . 0.5) "add9")))
	; major sixth chord with nine = 6/9
	(((0 . 0) (2 . 0) (4 . 0) (5 . 0) (1 . 0)) . (((raise . 0.5) "6/9")))

	;; minor chords
	; minor sixth chord = m6
 	(((0 . 0) (2 . -1) (4 . 0) (5 . 0)) . (columns("m")((raise . 0.5) "6")))
	;; minor major seventh chord = m triangle
	;; shouldn't this be a filled black triange, like this:  ? --jcn
	;;(((0 . 0) (2 . -1) (4 . 0) (6 . 0)) . (columns ("m") ((raise . 0.5)((font-family . math) "N"))))
	(((0 . 0) (2 . -1) (4 . 0) (6 . 0)) . (columns ("m") ((raise . 0.5)((font-family . math) "M"))))
	; minor seventh chord = m7
	(((0 . 0) (2 . -1) (4 . 0) (6 . -1)) . (columns("m")((raise . 0.5) "7")))
	; minor sixth nine chord = m6/9
	(((0 . 0) (2 . -1) (4 . 0) (5 . 0) (1 . 0)) . (columns("m")((raise . 0.5) "6/9")))
	; minor with added nine chord = madd9
	(((0 . 0) (2 . -1) (4 . 0) (1 . 0)) . (columns("m")((raise . 0.5) "add9")))
	; minor ninth chord = m9
	(((0 . 0) (2 . -1) (4 . 0) (6 . -1) (1 . 0)) . (columns("m")((raise . 0.5) "9")))

	;; dominant chords
	; dominant seventh = 7
	(((0 . 0) (2 . 0) (4 . 0) (6 . -1)) . (((raise . 0.5) "7")))
	; augmented dominant = +7
	;(((0 . 0) (2 . 0) (4 . +1) (6 . -1)) . (((raise . 0.5) "+7"))) ; +7 with both raised
	(((0 . 0) (2 . 0) (4 . +1) (6 . -1)) . (columns("+")((raise . 0.5) "7"))) ; +7 with 7 raised
	;(((0 . 0) (2 . 0) (4 . +1) (6 . -1)) . (columns((raise . 0.5) "7(")
	;	((raise . 0.3)(music (named ("accidentals-1"))))
	;	((raise . 0.5) "5)"))); 7(#5)
	; dominant flat 5 = 7(b5)
	(((0 . 0) (2 . 0) (4 . -1) (6 . -1)) . (columns((raise . 0.5) "7(")
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
	
 	;;(((0 . 0) (2 . -1) (4 . -1) (6 . -2)) . ((raise . 0.8) (size . -2) ("o")))
 	(((0 . 0) (2 . -1) (4 . -1) (6 . -2)) . ("" (super "o")))

	;; half diminshed chords
	;; half diminished seventh chord = slashed o
	;; (((0 . 0) (2 . -1) (4 . -1) (6 . -1)) . (((raise . 0.8) "/o")))
        (((0 . 0) (2 . -1) (4 . -1) (6 . -1)) . (columns (super (overstrike "o") "/") " " "7")) ; slashed o

	; half diminished seventh chord  with major 9 = slashed o cancelation 9
	(((0 . 0) (2 . -1) (4 . -1) (6 . -1) (1 . 0)) . (
		((raise . 0.8)"/o(")
		((raise . 0.3)(music (named ("accidentals-0"))))
		((raise . 0.8)"9)"))); 

;; Missing jazz chord definitions go here (note new syntax: see american for hints)

	)
      chord::names-alist-american))

(define (step->text-alternate-jazz pitch)
  (text-append
   (accidental->text (caddr pitch))
   (number->string (+ (cadr pitch) (if (= (car pitch) 0) 1 8)))))

(define (step->text-jazz pitch)
  (if (= (cadr pitch) 6)
      (case (caddr pitch)
	;; sharp 7 only included for completeness?
	((-2) (text-append (accidental->text -1) '("7")))
	((-1) '("7"))
	((0) '("maj7"))
	((1) (text-append (accidental->text-super 1) '("7")))
	((2) (text-append (accidental->text-super 2) '("7"))))
      (step->text-alternate-jazz pitch)))

(define (xchord::additions->text-jazz additions subtractions)
  (if (pair? additions)
      (text-append
       (let ((step (step->text-jazz (car additions))))
	 (if (or (pair? (cdr additions))
		 (pair? subtractions))
	     (text-append step "/")
	     step))
      (chord::additions->text-jazz (cdr additions) subtractions))
  '()))

(define (chord::>5? x)
  (or (> (car x) 0)
      (> (cadr x) 4)))


;; FIXME:
;; Perhaps all logic like this should be done earlier,
;; so that in this text-construction printing phase
;; we can just blindly create text from all additions.
;;
;; This depends maybe on the fact of code sharing,
;; in this layout, we can share the functions chord::additions
;; and chord::subtractions with banter.
(define (chord::additions->text-jazz additions subtractions)
  (text-append
   (chord::additions<=5->text-jazz (filter-out-list chord::>5? additions)
				   (filter-out-list chord::>5? subtractions))
   (chord::additions>5->text-jazz (filter-list chord::>5? additions)
				  (filter-list chord::>5? subtractions))))

;; FIXME
(define (chord::additions<=5->text-jazz additions subtractions)
  (let ((sus (chord::sus-four-jazz additions)))
    (if (pair? sus)
	(text-append '("sus") (step->text-jazz (car sus)))
	'())))

(define (chord::additions>5->text-jazz additions subtractions)
  "
Compose text of all additions

  * if there's a subtraction:
    - add `add'
    - list all up to highest
  * list all steps that are below an chromatically altered step
  "
  (text-append
   (if (not (empty? subtractions)) "add" '())
   (let ((radds (reverse additions)))
     (reverse (chord::additions>5->text-jazz-helper
	       radds
	       subtractions
	       (if (or (empty? subtractions) (empty? radds))
		   #f (car radds)))))))

(define (chord::additions>5->text-jazz-helper additions subtractions list-step)
  "
Create texts for all additions
If list-step != #f, list all steps down to 5
If we encounter a chromatically altered step, turn on list-step
"

  (if list-step
      (if (not (member list-step subtractions))
	  (if (> 5 (cadr list-step))
	      (cons (step->text-jazz list-step)
		    (chord::additions>5->text-jazz-helper
		     additions
		     subtractions
		     (chord::get-create-step additions
					     (- (cadr list-step) 2))))
	      (step->text-jazz list-step))
	  (chord::get-create-step additions (- (cadr list-step) 2)))
      (if (pair? additions)
	  (let ((step (car additions)))
	    (cons (step->text-jazz step)
		  (chord::additions>5->text-jazz-helper
		   (cdr additions)
		   subtractions
		   (if (or (and (!= 6 (cadr step)) (!= 0 (caddr step)))
			   (and (= 6 (cadr step)) (!= -1 (caddr step))))
		       (chord::get-create-step additions (- (cadr step) 2))
		       #f))))
	  '())))

(define (chord::sus-four-jazz chord-pitches)
  "List of pitches that are step 2 or step 4"
  (filter-list (lambda (x)
		 (and (= 0 (car x))
		      (or (= 1 (cadr x)) (= 3 (cadr x))))) chord-pitches))

(define (chord::get-create-step steps n)
  (let* ((i (if (< n 0) (+ n 7) n))
	 (found (filter-list (lambda (x) (= i (cadr x))) steps)))
    (if (empty? found)
	(if (!= i 6)
	    (list 0 i 0)
	    (list 0 6 -1))
	(car found))))
  
(define (chord::subtractions->text-jazz subtractions)	 
  (if (pair? subtractions)
      (text-append
       (if (= 5 (cadr (car subtractions)))
	   (text-append
	    '("omit")
	    (let ((step (step->text-jazz (car subtractions))))
	      (if (pair? (cdr subtractions))
		  (text-append step "/")
		  step)))
	   '())
       (chord::subtractions->text-jazz (cdr subtractions)))
      '()))


;; TODO: maybe merge with inner-name-banter
;; Combine tonic, exception-part of chord name,
;; additions, subtractions and bass or inversion into chord name
(define (chord::inner-name-jazz tonic exception-part additions subtractions
				  bass-and-inversion steps)
    (text-append
     (pitch->chord-name-text-banter tonic steps)
     exception-part
     ;; why does list->string not work, format seems only hope...
     (if (and (string-match "super" (format "~s" exception-part))
	      (or (pair? additions)
		  (pair? subtractions)))
	 (list simple-super "/"))
     
     (list `(,simple-super)
	   (chord::additions->text-jazz additions subtractions)
	   (chord::subtractions->text-jazz subtractions))
     (chord::bass-and-inversion->text-banter bass-and-inversion)))

;; Jazz style--basically similar to american with minor changes
;;
;; Consider Dm6.  When we get here:
;;     tonic =  '(0 1 0) (note d=2)
;;     steps =  '((0 0 0) '(0 2 -1) (0 4 0) (0 5 0))
;;               steps are transposed for tonic c, octave 0,
;;               so (car steps) is always (0 0 0)
;;     except  = ("m")
;;               assuming that the exceptions-alist has an entry
;;               '(((0 . 0) (2 . -1)) . ("m"))
;;               (and NOT the full chord, like std jazz list, ugh)
;;     unmatch = '((0 0 0) (0 2 0) (0 4 0) (0 5 0))
;;     subtract= '()
;;
;; You can look very easily what happens, if you add some write-me calls,
;; and run lilypond on a simple file, eg, containing only the chord c:m6:
;;
;;   (let ((additions (write-me "adds: "
;;                 (chord::additions (write-me "unmatched:"
;;                 unmatched-steps))))
;;
;; If you set subtract #f, the chord::inner-name-jazz does not see any
;; subtractions, ever, so they don't turn up in the chord name.
;;
(define (chord::name-jazz tonic exception-part unmatched-steps
			  bass-and-inversion steps)
  (let ((additions (chord::additions unmatched-steps))
	;; get no 'omit' or 'no'
	;; (subtractions #f))
    	(subtractions (chord::subtractions unmatched-steps)))
    (chord::inner-name-jazz tonic exception-part additions subtractions
	     bass-and-inversion steps)))

;; wip (set! chord::names-alist-jazz
(define chord::names-alist-jazz
      (append
      '(
        (((0 . 0) (2 . -1)) . ("m"))

	;; some fixups -- jcn
	; major seventh chord = triangle
	(((0 . 0) (2 . 0) (4 . 0) (6 . 0)) .  (((raise . 0.5)((font-family . math) "N"))))
	;; (((0 . 0) (2 . 0) (4 . 0) (6 . 0)) .  (((raise . 0.5)((font-family . math) "M"))))

	;; minor major seventh chord = m triangle
	(((0 . 0) (2 . -1) (4 . 0) (6 . 0)) . (columns ("m") ((raise . 0.5)((font-family . math) "N"))))
	;; (((0 . 0) (2 . -1) (4 . 0) (6 . 0)) . (columns ("m") ((raise . 0.5)((font-family . math) "M"))))
	
	)
      ;; '()))
      chord::names-alist-american))
