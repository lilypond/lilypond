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


;; debugging.
;;(define (write-me x) (write x) (newline) x)
(define (write-me x) x)
;;(define (write-me x) (write x) (newline) x)
;;(define (write-me-2 x y) (write "FOO") (write x) (write y) (newline) y)


"
TODO:

- Use lilypond Pitch objects -- SCM pitch objects lead to
duplication. LilyPond pitch objects force meaningful names
(i.e. (ly:pitch-octave PITCH)  )

- Pitches are musical objects. The pitches -> markup step should
happen earlier (during interpreting), brew-molecule () should only
dump reinterpret the markup as a molecule. " ; "


;; pitch = (octave notename alteration)
;;
;; note = (notename . alteration)
;;
;; text = scm markup text -- see font.scm and input/test/markup.ly


;; TODO

;; Ugh : naming chord::... ; this is scheme not C++
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

(define chord::names-alist-banter
       `(
	; C iso C.no3.no5
	(((0 . 0)) . ,empty-markup)
	; C iso C.no5
	(((0 . 0) (2 . 0)) . ,empty-markup)
	; Cm iso Cm.no5
	(((0 . 0) (2 . -1)) . ,(make-simple-markup "m"))
	; C2 iso C2.no3
	(((0 . 0) (1 . 0) (4 . 0))
	 . ,(make-super-markup (make-simple-markup "2 ")))
	; C4 iso C4.no3
	(((0 . 0) (3 . 0) (4 . 0))
	 . ,(make-super-markup (make-simple-markup "4 ")))
	;; Cdim iso Cm5-
	(((0 . 0) (2 . -1) (4 . -1)) . ,(make-simple-markup "dim"))
	; URG: Simply C:m5-/maj7 iso Cdim maj7
	(((0 . 0) (2 . -1) (4 . -1) (6 . 0))
	 . ,(make-line-markup
	     (list
	      (make-simple-markup "m")
	      (make-super-markup (make-simple-markup "5-/maj7 ")))))
	; URG: Simply C:m5-/7 iso Cdim7
	(((0 . 0) (2 . -1) (4 . -1) (6 . -1))
	 . ,(make-line-markup
	     (list
	     (make-simple-markup "m")
	     (make-super-markup (make-simple-markup "5-/7 ")))))
	; Co iso C:m5-/7-
        (((0 . 0) (2 . -1) (4 . -1) (6 . -2))
	 . ,(make-super-markup (make-simple-markup "o ")))
	; Cdim9
	(((0 . 0) (2 . -1) (4 . -1) (6 . -2) (1 . -1))
	 . ,(make-line-markup
	     (list (make-simple-markup "dim")
		   (make-simple-markup "9 "))))
	(((0 . 0) (2 . -1) (4 . -1) (6 . -2) (1 . -1) (3 . -1))
	 . ,(make-line-markup
	     (list (make-simple-markup "dim")
				(make-super-markup
				 (make-simple-markup "11 ")))))
	
	))

;;;;;;;;;;

(define (pitch->note-name pitch)
  (cons (cadr pitch) (caddr pitch)))

(define (accidental-markup acc)
  "ACC is an int, return a markup making an accidental."
  (if (= acc 0)
      (make-line-markup (list empty-markup))
      (make-smaller-markup (make-musicglyph-markup
			    (string-append "accidentals-"
					   (number->string acc))))))

(define (pitch->markup pitch)
  (make-line-markup
   (list
    (make-simple-markup
     (make-string 1 (integer->char (+ (modulo (+ (cadr pitch) 2) 7) 65))))
    ;; undefined?
    ;; (make-normal-size-superscript-markup
    (make-super-markup
     (accidental-markup (caddr pitch))))))
  
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

;; markup: list of word
;; word: string + optional list of property
;; property: axis, kern, font (?), size

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
	(let ((step (step->markup-jazz (car subtractions))))
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
;;        call (chord::restyle 'chord::bass-and-inversion->markup- style)
;;        See: chord::exceptions-lookup
(define (chord::inner-name-banter tonic exception-part additions subtractions
				  bass-and-inversion steps)

  "
        
 Banter style
 Combine tonic, exception-part of chord name,
 additions, subtractions and bass or inversion into chord name

"
  (let* ((tonic-markup (pitch->chord-name-markup-banter tonic steps))
	 (except-markup

	  (if exception-part exception-part empty-markup))  ;;(make-simple-markup "")))
	 (sep-markup (make-simple-markup
		      (if (and (string-match "super"
					     (format "~s" except-markup))
			       (or (pair? additions)
				   (pair? subtractions)))
			  "/" "")))
	 (adds-markup (chord::additions->markup-banter additions subtractions))
	 (subs-markup (chord::subtractions->markup-banter subtractions))
	 (b+i-markup (chord::bass-and-inversion->markup-banter
		      bass-and-inversion)))
    
    (make-line-markup
     (list
      tonic-markup
      except-markup
      sep-markup
      (make-raise-markup
       0.3
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


(define chord-module (current-module))
(define (chord::restyle name style)
  ;;   "UGGHGUGHUGHG"
  (eval
   (string->symbol
    (string-append (symbol->string name)
		   (symbol->string style)))
   chord-module
   ))


;; this is unintelligible.
;;

;
; - what's a helper, and why isn't it inside another function?
;
; what is going out, what is coming in, howcome it produces #f 
;  in some cases?
;

(define (chord::exceptions-lookup-helper
	 exceptions-alist try-steps unmatched-steps exception-part)
	 "

 check exceptions-alist for biggest matching part of try-steps
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

;; see above.

(define (chord::exceptions-lookup style steps)
  "
   return (MATCHED-EXCEPTION . BASE-CHORD-WITH-UNMATCHED-STEPS)
   BASE-CHORD-WITH-UNMATCHED-STEPS always includes (tonic 3 5)

"

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


(define (chord::name->markup style tonic steps bass-and-inversion)
  (write-me tonic)
  (write-me steps)
  (let* ((lookup (write-me (chord::exceptions-lookup style steps)))
	 (exception-part (write-me (car lookup)))
	 (unmatched-steps (cadr lookup))
	 (func (chord::restyle 'chord::name- style))
	 )

    
    (func tonic exception-part unmatched-steps bass-and-inversion steps)))

;; C++ entry point
;; 
;; Check for each subset of chord, full chord first, if there's a
;; user-override.  Split the chord into user-overridden and to-be-done
;; parts, complete the missing user-override matched part with normal
;; chord to be name-calculated.
;;
;; CHORD: (pitches (bass . inversion))
(define-public (chord->markup style chord)
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
    
    (chord::name->markup style (car pitches) steps bass-and-inversion)))

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


(define chord::names-alist-american
      
       `(
	 (((0 . 0)) . ,empty-markup)
	 (((0 . 0) (2 . 0)) . ,empty-markup)
	 ;; Root-fifth chord
	 (((0 . 0) (4 . 0)) . ,(make-simple-markup "5"))
	 ;; Common triads
	 (((0 . 0) (2 . -1)) . ,(make-simple-markup  "m"))
	 (((0 . 0) (3 . 0) (4 . 0)) . ,(make-simple-markup "sus"))
	 (((0 . 0) (2 . -1) (4 . -1)) . ,(make-simple-markup "dim"))
;Alternate:	 (((0 . 0) (2 . -1) (4 . -1)) . ("" (super "o")))
	 (((0 . 0) (2 . 0) (4 . 1)) . ,(make-simple-markup "aug"))
;Alternate:	 (((0 . 0) (2 . 0) (4 . 1)) . ("+"))
	 (((0 . 0) (1 . 0) (4 . 0)) . ,(make-simple-markup "2"))
	 ;; Common seventh chords
	 (((0 . 0) (2 . -1) (4 . -1) (6 . -2)) .
	  ,(make-line-markup
	    (list
	     (make-super-markup (make-simple-markup "o"))
	     (make-simple-markup " 7"))))
	 (((0 . 0) (2 . 0) (4 . 0) (6 . 0)) . ,(make-simple-markup "maj7"))
	 ;; urg! should use (0 . 0 2 . -1) -> "m", and add "7" to that!!
	 (((0 . 0) (2 . -1) (4 . 0) (6 . -1)) . ,(make-simple-markup "m7"))
	 (((0 . 0) (2 . 0) (4 . 0) (6 . -1)) . ,(make-simple-markup "7"))
	 (((0 . 0) (2 . -1) (4 . 0) (6 . 0)) . ,(make-simple-markup "m(maj7)"))
	 ;jazz: the delta, see jazz-chords.ly
	 ;;(((0 . 0) (2 . -1) (4 . -1) (6 . -2))
	 ;; .  (super ((font-family . math) "N"))
	 ;; slashed o
	 (((0 . 0) (2 . -1) (4 . -1) (6 . -1)) .
	  ,(make-line-markup
	    (list
	     (make-super-markup
	      (make-combine-markup (make-simple-markup "o")
				   (make-simple-markup "/")))
	     (make-simple-markup " 7"))))
	 (((0 . 0) (2 . 0) (4 . 1) (6 . -1)) . ,(make-simple-markup "aug7"))
	 (((0 . 0) (2 . 0) (4 . -1) (6 . 0))
	  . ,(make-line-markup
	      (list
	       (make-simple-markup "maj7")
	       (make-small-markup
		(make-raise-markup 0.2 (accidental-markup -1)))
	       (make-simple-markup "5"))))
	 (((0 . 0) (2 . 0) (4 . -1) (6 . -1)) .
	  ,(make-line-markup
	    (list
	     (make-simple-markup "7")
	     (make-small-markup (make-raise-markup 0.2 (accidental-markup -1)))
	     (make-simple-markup "5"))))
	 (((0 . 0) (3 . 0) (4 . 0) (6 . -1)) . ,(make-simple-markup "7sus4"))
	 ;; Common ninth chords
	 (((0 . 0) (2 . 0) (4 . 0) (5 . 0) (1 . 0))
	  . ,(make-simple-markup "6/9")) ;; we don't want the '/no7'
	 (((0 . 0) (2 . 0) (4 . 0) (5 . 0)) . ,(make-simple-markup "6"))
	 (((0 . 0) (2 . -1) (4 . 0) (5 . 0)) . ,(make-simple-markup "m6"))
	 (((0 . 0) (2 . 0) (4 . 0) (1 . 0)) . ,(make-simple-markup "add9"))
	 (((0 . 0) (2 . 0) (4 . 0) (6 . 0) (1 . 0))
	  . ,(make-simple-markup "maj9"))
	 (((0 . 0) (2 . 0) (4 . 0) (6 . -1) (1 . 0))
	  . ,(make-simple-markup "9"))
	 (((0 . 0) (2 . -1) (4 . 0) (6 . -1) (1 . 0))
	  . ,(make-simple-markup "m9"))

	 ))

;; American style chordnames use no "no",
;; but otherwise very similar to banter for now
(define-public (chord::name-american tonic exception-part unmatched-steps
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

(define mathm-markup-object
  (make-override-markup '(font-family . math) (make-simple-markup "M")))

(define mraise-arg `(make-line-markup
		     (list
		      ,(make-simple-markup "m")
		      (make-raise-markup 0.5 (make-simple-markup arg)))))

(define (raise-some-for-jazz arg-list)
  (define (do-one x)
    (case x
      ("@"  (make-raise-markup 0.3 ,(accidental-markup -1)))
      ("#"  (make-raise-markup 0.3 ,(accidental-markup 1)))
      (else (make-raise-markup 0.8 ,x))))

  (make-line-markup
   (list (map do-one arg-list))))

(define chord::names-alist-jazz 
  (append 
      '(
	;; major chords
	; major sixth chord = 6
 	(((0 . 0) (2 . 0) (4 . 0) (5 . 0)) .
	 ,(make-raise-markup 0.5 (make-simple-markup "6")))
	; major seventh chord = triangle
	;; shouldn't this be a filled black triange, like this:  ? --jcn
	;; (((0 . 0) (2 . 0) (4 . 0) (6 . 0)) .  (((raise . 0.5)((font-family . math) "N"))))
	(((0 . 0) (2 . 0) (4 . 0) (6 . 0)) .
	 ,(make-raise-markup 0.5 mathm-markup-object))
	
	; major chord add nine = add9
	(((0 . 0) (2 . 0) (4 . 0) (1 . 0))
	 . ,(make-raise-markup 0.5 (make-simple-markup "add9")))
	; major sixth chord with nine = 6/9
	(((0 . 0) (2 . 0) (4 . 0) (5 . 0) (1 . 0))
	 . ,(make-raise-markup 0.5 (make-simple-markup "add9")))

	;; minor chords
	; minor sixth chord = m6
 	(((0 . 0) (2 . -1) (4 . 0) (5 . 0)) .
	 ,(mraise-arg "6"))

	;; minor major seventh chord = m triangle
	;; shouldn't this be a filled black triange, like this:  ? --jcn
	;;(((0 . 0) (2 . -1) (4 . 0) (6 . 0)) . (columns ("m") ((raise . 0.5)((font-family . math) "N"))))
	(((0 . 0) (2 . -1) (4 . 0) (6 . 0)) .
	 ,(make-line-markup
	   (list ((make-simple-markup "m") mathm-markup-object))))
	; minor seventh chord = m7
	(((0 . 0) (2 . -1) (4 . 0) (6 . -1)) . ,(mraise-arg "7"))
	; minor sixth nine chord = m6/9
	(((0 . 0) (2 . -1) (4 . 0) (5 . 0) (1 . 0)) . ,(mraise-arg "6/9"))

					; minor with added nine chord = madd9
	(((0 . 0) (2 . -1) (4 . 0) (1 . 0)) . ,(mraise-arg "madd9"))

					; minor ninth chord = m9
	(((0 . 0) (2 . -1) (4 . 0) (6 . -1) (1 . 0)) . ,(mraise-arg "add9"))

	;; dominant chords
	; dominant seventh = 7
	(((0 . 0) (2 . 0) (4 . 0) (6 . -1))
	 . ,(make-raise-markup 0.5 (make-simple-markup "7")))
	; augmented dominant = +7
	;(((0 . 0) (2 . 0) (4 . +1) (6 . -1)) . (((raise . 0.5) "+7"))) ; +7 with both raised
	(((0 . 0) (2 . 0) (4 . +1) (6 . -1)) .
	 ,(make-line-markup
	   (list
	    (make-simple-markup "+")
	    ;; +7 with 7 raised
	    (make-raise-markup 0.5 (make-simple-markup "7")))))
	;(((0 . 0) (2 . 0) (4 . +1) (6 . -1)) . (columns((raise . 0.5) "7(")
	;	((raise . 0.3)(music (named ("accidentals-1"))))
	;	((raise . 0.5) "5)"))); 7(#5)
	; dominant flat 5 = 7(b5)
	
	(((0 . 0) (2 . 0) (4 . -1) (6 . -1))
	 . ,(raise-some-for-jazz '( "7(" "@" "5)" )))
	
					; dominant 9 = 7(9)
	(((0 . 0) (2 . 0) (4 . 0) (6 . -1) (1 . 0)) .
	 ,(raise-some-for-jazz '("7(9)")))
	; dominant flat 9 = 7(b9)
	(((0 . 0) (2 . 0) (4 . 0) (6 . -1) (1 . -1)) .
	 ,(raise-some-for-jazz '("7(" "@" "9)")))
	
	; dominant sharp 9 = 7(#9)
	(((0 . 0) (2 . 0) (4 . 0) (6 . -1) (1 . +1)) .
	 ,(raise-some-for-jazz '("7(" "#" "9)")))

					; dominant 13 = 7(13)
	(((0 . 0) (2 . 0) (4 . 0) (6 . -1) (5 . 0)) .
	 ,(raise-some-for-jazz "7(13)"))
	; dominant flat 13 = 7(b13)
	(((0 . 0) (2 . 0) (4 . 0) (6 . -1) (5 . -1)) .
	 ,(raise-some-for-jazz '( "7(" "@" "13)")))

					; dominant 9, 13 = 7(9,13)
	(((0 . 0) (2 . 0) (4 . 0) (6 . -1) (1 . 0) (5 . 0)) .
	 ,(raise-some-for-jazz '("7(9, 13)")))
	; dominant flat 9, 13 = 7(b9,13)
	(((0 . 0) (2 . 0) (4 . 0) (6 . -1) (1 . -1) (5 . 0)) .
	 ,(raise-some-for-jazz '("7(" "@" "9, 13)")))
	
	; dominant sharp 9, 13 = 7(#9,13)
	(((0 . 0) (2 . 0) (4 . 0) (6 . -1) (1 . +1) (5 . 0)) .
	 ,(raise-some-for-jazz '("7(" "#" "9,13)")))

					; dominant 9, flat 13 = 7(9,b13)
	(((0 . 0) (2 . 0) (4 . 0) (6 . -1) (1 . 0) (5 . -1)) .
	 ,(raise-some-for-jazz "7(9, " "@" "13)"))
	
	; dominant flat 9, flat 13 = 7(b9,b13)
	(((0 . 0) (2 . 0) (4 . 0) (6 . -1) (1 . -1) (5 . -1)) .
	 ,(raise-some-for-jazz '("7(" "@" "9, " "@" "13)")))
	 
	; dominant sharp 9, flat 13 = 7(#9,b13)
	(((0 . 0) (2 . 0) (4 . 0) (6 . -1) (1 . +1) (5 . -1)) .
	 ,(raise-some-for-jazz '("7(" "#" "9, " "@" "13)")))

	;; diminished chord(s)
	; diminished seventh chord =  o


	;; DONT use non-ascii characters, even if ``it works'' in Windows
	
 	;;(((0 . 0) (2 . -1) (4 . -1) (6 . -2)) . ((raise . 0.8) (size . -2) ("o")))
 	(((0 . 0) (2 . -1) (4 . -1) (6 . -2)) .
	 ,(make-super-markup (make-simple-markup "o")))

	;; half diminshed chords
	;; half diminished seventh chord = slashed o
	;; (((0 . 0) (2 . -1) (4 . -1) (6 . -1)) . (((raise . 0.8) "/o")))
        (((0 . 0) (2 . -1) (4 . -1) (6 . -1)) .
	 ,(make-line-markup
	   (list
	    (make-super-markup
	     (make-combine-markup
	      (make-simple-markup "o") (make-simple-markup "/")))
	    (make-simple-markup "  7"))))
	; half diminished seventh chord  with major 9 = slashed o cancelation 9
	(((0 . 0) (2 . -1) (4 . -1) (6 . -1) (1 . 0)) .
	 ,(raise-some-for-jazz '("/o(" "!" "9)")))

;; Missing jazz chord definitions go here (note new syntax: see american for hints)

	)
      chord::names-alist-american))

(define (step->markup-alternate-jazz pitch)
  (make-line-markup
   (list
    (accidental-markup (caddr pitch))
    (make-simple-markup
     (number->string (+ (cadr pitch) (if (= (car pitch) 0) 1 8)))))))

(define (step->markup-jazz pitch)
  (if (= (cadr pitch) 6)
      (case (caddr pitch)
	;; sharp 7 only included for completeness?
	((-2) (make-line-markup
	       (list
		(accidental-markup  -1)
		(make-simple-markup "7"))))
	((-1) (make-simple-markup "7"))
	((0) (make-simple-markup "maj7"))
	;;((0) (make-line-markup
	;;      (list (make-simple-markup "maj7"))))
	((1) (make-line-markup
	      (list
	       (accidental-markup 1) (make-simple-markup "7"))))
	((2) (make-line-markup
	      (list (accidental-markup 1)
		    (make-simple-markup "7")))))
      (step->markup-alternate-jazz pitch)))

;; removeme ?
(define (xchord::additions->markup-jazz additions subtractions)
  (if (pair? additions)
      (make-line-markup
       (list
	(let ((step (step->markup-jazz (car additions))))
	  (if (or (pair? (cdr additions))
		  (pair? subtractions))
	      (make-line-markup (list step (make-simple-markup "/")))
	      step))
	(chord::additions->markup-jazz (cdr additions) subtractions)))
      empty-markup))

(define (chord::>5? x)
  (or (> (car x) 0)
      (> (cadr x) 4)))


;; FIXME:
;; Perhaps all logic like this should be done earlier,
;; so that in this markup-construction printing phase
;; we can just blindly create markup from all additions.
;;
;; This depends maybe on the fact of code sharing,
;; in this layout, we can share the functions chord::additions
;; and chord::subtractions with banter.
(define (chord::additions->markup-jazz additions subtractions)
      ;; FIXME
  (make-line-markup
   (list
    (chord::additions<=5->markup-jazz
     (filter-out-list chord::>5? additions)
     (filter-out-list chord::>5? subtractions))
    (chord::additions>5->markup-jazz
     (filter-list chord::>5? additions)
     (filter-list chord::>5? subtractions)))))


;; FIXME
(define (chord::additions<=5->markup-jazz additions subtractions)
  (let ((sus (chord::sus-four-jazz additions)))
    (if (pair? sus)
	(make-line-markup
	 (list (make-simple-markup "sus")
	       (step->markup-jazz (car sus))))
	empty-markup)))


(define (chord::additions>5->markup-jazz additions subtractions)
  "
Compose markup of all additions

  * if there's a subtraction:
    - add `add'
    - list all up to highest
  * list all steps that are below an chromatically altered step
  "
  
  (make-line-markup
   (list
    (if (not (null? subtractions))
	(make-simple-markup "add")
	empty-markup)
    ;; this is totally incomprehensible. Fix me, and docme.

    ;; The function >5markup-jazz-helper cdrs through the list
    ;; of additions in reverse order, ie, for c 7 9+:
    ;;   (1 1 1), (0 6 0),  done
    
    ;; For each step, it creates a markup, if necessary, and
    ;; cons's it to the list.
    
    ;; The list is reversed.
    (let* ((radds (reverse additions))
	   (rmarkups (chord::additions>5->markup-jazz-helper
		      radds
		      subtractions
		      (if (or (null? subtractions) (null? radds))
			  #f (car radds)))))
      (if (null? rmarkups)
	  empty-markup
	  (make-line-markup (reverse rmarkups)))))))
	      


(define (chord::additions>5->markup-jazz-helper additions subtractions list-step)
  "
Create markups for all additions
If list-step != #f, list all steps down to 5
If we encounter a chromatically altered step, turn on list-step
"

  (if list-step
      (if (not (member list-step subtractions))
	  (if (> 5 (cadr list-step))
	      (cons
	       (step->markup-jazz list-step)
	       
	       (chord::additions>5->markup-jazz-helper
		additions
		subtractions
		(chord::get-create-step additions
					(- (cadr list-step) 2))))
	      
	      (list (step->markup-jazz list-step)))

	  '())
      
      (if (pair? additions)
	  (let ((step (car additions)))
	    (cons
	     (step->markup-jazz step)
	     
	     (chord::additions>5->markup-jazz-helper
	      (cdr additions)
	      subtractions
	      (if ;;; possible fix --jcn
	       (and list-step
		    (or (and (!= 6 (cadr step)) (!= 0 (caddr step)))
			(and (= 6 (cadr step)) (!= -1 (caddr step))))
		    ) ;;; possible fix --jcn
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
    (if (null? found)
	(if (!= i 6)
	    (list 0 i 0)
	    (list 0 6 -1))
	(car found))))
  
(define (chord::subtractions->markup-jazz subtractions)	 
  (if (pair? subtractions)
      (make-line-markup
       (list
	(if (= 5 (cadr (car subtractions)))
	    (make-line-markup
	     (list
	      (make-simple-markup "omit")
		 (let ((step (step->markup-jazz (car subtractions))))
		   (if (pair? (cdr subtractions))
		       (make-line-markup
			(list (step (make-simple-markup "/"))))
		       step))))
	    empty-markup)
	(chord::subtractions->markup-jazz (cdr subtractions))))
      empty-markup))

;; TODO: maybe merge with inner-name-banter
;; Combine tonic, exception-part of chord name,
;; additions, subtractions and bass or inversion into chord name
(define (chord::inner-name-jazz tonic exception-part additions subtractions
				  bass-and-inversion steps)
  (make-line-markup
   (list
    (pitch->chord-name-markup-banter tonic steps)
    exception-part
    ;; why does list->string not work, format seems only hope...
    (if (and (string-match "super" (format "~s" exception-part))
	     (or (pair? additions)
		 (pair? subtractions)))
	(make-super-markup (make-simple-markup "/"))
	empty-markup)
   
    (make-super-markup
     (make-line-markup
      (list
       (chord::additions->markup-jazz additions subtractions)
       (chord::subtractions->markup-jazz subtractions))))
      
   (chord::bass-and-inversion->markup-banter bass-and-inversion))))

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
(define-public (chord::name-jazz tonic exception-part unmatched-steps
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
       `(
        (((0 . 0) (2 . -1)) . ,(make-simple-markup "m"))

	;; some fixups -- jcn
	; major seventh chord = triangle
	(((0 . 0) (2 . 0) (4 . 0) (6 . 0))
	 . ,(make-raise-markup 0.5 mathm-markup-object))

	 ;; minor major seventh chord = m triangle
	(((0 . 0) (2 . -1) (4 . 0) (6 . 0))
	 . ,(make-line-markup
	     (list
	     (make-simple-markup "m")
	     (make-raise-markup 0.5 mathm-markup-object))))
	;; (((0 . 0) (2 . -1) (4 . 0) (6 . 0))
	;; . (columns ("m") ((raise . 0.5)((font-family . math) "M"))))
	
	)
      ;; '()))
      chord::names-alist-american))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-public (new-chord-name-brew-molecule grob)
  (let*
      (
       (style-prop (ly:get-grob-property grob 'style))
       (style (if (symbol? style-prop) style-prop  'banter))
       (chord (ly:get-grob-property grob 'chord))
       (chordf (ly:get-grob-property grob 'chord-name-function))
       (ws (ly:get-grob-property grob 'word-space))
       (markup (chordf style chord))
       (molecule (interpret-markup grob
				   (cons '((word-space . 0.0))
					 (Font_interface::get_property_alist_chain grob))
				   markup))
       )


    ;;;  TODO: BUG : word-space is in local staff-space (?)
    (if (number? ws)
	(ly:combine-molecule-at-edge  molecule
	 X RIGHT (ly:make-molecule "" (cons 0 ws) '(-1 . 1) )
	 0.0)
	molecule)
	))

