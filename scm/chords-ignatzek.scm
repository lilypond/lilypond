(define (natural-chord-alteration p)
  "Return the natural alteration for step P."
  (if (= (ly:pitch-steps p) 6)
      -1
      0))


(define-public (alteration->text-accidental-markup alteration)
  (make-smaller-markup
   (make-raise-markup
    (if (= alteration -1)
	0.3
	0.6)
    (make-musicglyph-markup
     (string-append "accidentals-" (number->string alteration))))))
  
(define (accidental->markup alteration)
  "Return accidental markup for ALTERATION."
  (if (= alteration 0)
      (make-line-markup (list empty-markup))
      (conditional-kern-before
       (alteration->text-accidental-markup alteration)
       (= alteration -1) 0.2
       )))


(define-public (note-name->markup pitch)
  "Return pitch markup for PITCH."
  (make-line-markup
   (list
    (make-simple-markup
     (vector-ref #("C" "D" "E" "F" "G" "A" "B") (ly:pitch-notename pitch)))
    (make-normal-size-super-markup
     (accidental->markup (ly:pitch-alteration pitch))))))


(define-public ((chord-name->german-markup B-instead-of-Bb) pitch)
  "Return pitch markup for PITCH, using german note names.
   If B-instead-of-Bb is set to #t real german names are returned.
   Otherwise semi-german names (with Bb and below keeping the british names)
"
  (let* ((name (ly:pitch-notename pitch))
         (alt (ly:pitch-alteration pitch))
	 (n-a (if (member (cons name alt) '((6 . -1) (6 . -2)))
		 (cons 7 (+ (if B-instead-of-Bb 1 0) alt))
		 (cons name alt))))
    (make-line-markup
     (list
      (make-simple-markup
       (vector-ref #("C" "D" "E" "F" "G" "A" "H" "B") (car n-a)))
      (make-normal-size-super-markup
       (accidental->markup (cdr n-a)))))))


(define-public (note-name->german-markup  pitch)
  (let* ((name (ly:pitch-notename pitch))
	 (alt (ly:pitch-alteration pitch))
	 (n-a (if (member (cons name alt) '((6 . -1) (6 . -2)))
		  (cons 7 (+ 1 alt))
		  (cons name alt))))
    (make-line-markup
     (list
      (string-append
       (list-ref '("c" "d" "e" "f" "g" "a" "h" "b") (car n-a))
       (if (or (equal? (car n-a) 2) (equal? (car n-a) 5))
	   (list-ref '( "ses"  "s" "" "is" "isis") (+ 2 (cdr n-a)))
	   (list-ref '("eses" "es" "" "is" "isis") (+ 2 (cdr n-a)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define-public (sequential-music-to-chord-exceptions seq)
  "Transform sequential music of <<a b c>>-\markup{ foobar } type to
 (cons ABC-PITCHES FOOBAR-MARKUP)
 "
  
  (define (is-req-chord? m)
    (and
     (memq 'event-chord (ly:get-mus-property m 'types))
     (not (equal? (ly:make-moment 0 1) (ly:get-music-length m)))
    ))

  (define (chord-to-exception-entry m)
    (let*
	(
	 (elts   (ly:get-mus-property m 'elements))
	 (pitches (map
		   (lambda (x)
		     (ly:get-mus-property x 'pitch)
		     )
		   (filter-list
		    (lambda (y) (memq 'note-event (ly:get-mus-property y 'types)))
		    elts)))
	 (sorted  (sort pitches ly:pitch<? ))
	 (root (car sorted))
	 (non-root (map (lambda (x) (ly:pitch-diff x root)) (cdr sorted)))
	 (texts (map
		 (lambda (x)
		   (ly:get-mus-property x 'text)
		   )
		 
		 (filter-list
		  (lambda (y)
		    (memq 'text-script-event
			  (ly:get-mus-property y 'types))) elts)
		 ))
	 (text (if (null? texts)
		   #f
		   (car texts)))

	 )
      (cons non-root text)
    ))

  (let*
    (
     (elts (filter-list is-req-chord? (ly:get-mus-property seq 'elements)))
     (alist (map chord-to-exception-entry elts))
     )
    (filter-list (lambda (x) (cdr x)) alist)
  ))


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
;; the split is a procedural process, with lots of set!. 
;;


;; todo: naming is confusing: steps  (0 based) vs. steps (1 based).
(define (pitch-step p)
  "Musicological notation for an interval. Eg. C to D is 2."
  (+ 1 (ly:pitch-steps p)))

(define (get-step x ps)
  "Does PS have the X step? Return that step if it does."
  (if (null? ps)
      #f
      (if (= (- x 1) (ly:pitch-steps (car ps)))
	  (car ps) 
	  (get-step x (cdr ps)))
      ))

(define (replace-step p ps)
  "Copy PS, but replace the step of P in PS."
  (if (null? ps)
      '()
      (let*
	  (
	   (t (replace-step p (cdr ps)))
	   )

	(if (= (ly:pitch-steps p) (ly:pitch-steps (car ps)))
	    (cons p t)
	    (cons (car ps) t)
	    ))
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


(define-public (ignatzek-chord-names
		in-pitches bass inversion
		context)

  (define (remove-uptil-step x ps)
    "Copy PS, but leave out everything below the Xth step."
    (if (null? ps)
	'()
	(if (< (ly:pitch-steps (car ps)) (- x 1))
	    (remove-uptil-step x (cdr ps))
	    ps)
	)
    )
  (define name-root (ly:get-context-property context 'chordRootNamer))
  (define name-note 
    (let ((nn (ly:get-context-property context 'chordNoteNamer)))
      (if (eq? nn '())
	  ; replacing the next line with name-root gives guile-error...? -rz
	  name-root
	  nn)))

  (define (is-natural-alteration? p)
    (= (natural-chord-alteration p)  (ly:pitch-alteration p))
    )
  
  
  (define (ignatzek-format-chord-name
	   root
	   prefix-modifiers
	   main-name
	   alteration-pitches
	   addition-pitches
	   suffix-modifiers
	   bass-pitch
	   )

    "Format for the given (lists of) pitches. This is actually more
work than classifying the pitches."
    
    (define (filter-main-name p)
    "The main name: don't print anything for natural 5 or 3."
    (if
     (or (not (ly:pitch? p))
	 (and (is-natural-alteration? p)
	  (or (= (pitch-step p) 5)
	      (= (pitch-step p) 3))))
     '()
     (list (name-step p))
     ))

    (define (glue-word-to-step word x)
      (make-line-markup 
       (list
	(make-simple-markup word)
	(name-step x)))
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
      "Filter out uninteresting (natural) pitches from ALTERS."
      
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

    (define (name-step pitch)
      (define (step-alteration pitch)
	(- (ly:pitch-alteration pitch)
	   (natural-chord-alteration pitch)
	   ))

      (let*
	  (
	   (num-markup (make-simple-markup
			(number->string (pitch-step pitch))))
	   (args (list num-markup))
	   (total (if (= (ly:pitch-alteration pitch) 0)
		      (if (= (pitch-step pitch) 7)
			  (list (ly:get-context-property context 'majorSevenSymbol))
			  args)
		      (cons (accidental->markup (step-alteration pitch)) args)
		      ))
	   )
	
	(make-line-markup total)))

    (let*
	(
	 (sep (ly:get-context-property context 'chordNameSeparator))
	 (root-markup (name-root root))
	 (add-markups (map (lambda (x)
			     (glue-word-to-step "add" x))
			   addition-pitches))
	 (filtered-alterations (filter-alterations alteration-pitches))
	 (alterations (map name-step filtered-alterations))
	 (suffixes (map suffix-modifier->markup suffix-modifiers))
	 (prefixes (map prefix-modifier->markup prefix-modifiers))
	 (main-markups (filter-main-name main-name))
	 (to-be-raised-stuff (markup-join
			      (append
			       main-markups
			       alterations
			       suffixes
			       add-markups) sep))
	 (base-stuff (if bass-pitch
			 (list sep (name-note bass-pitch))
			 '()))
	 )

      (set! base-stuff
	    (append
	     (list root-markup
		   (markup-join prefixes sep)
		   (make-super-markup to-be-raised-stuff))
	     base-stuff))
      (make-line-markup       base-stuff)

       ))

  (let*
      (
       (root (car in-pitches))
       (pitches (map (lambda (x) (ly:pitch-diff x root)) (cdr in-pitches)))
       (exceptions (ly:get-context-property context 'chordNameExceptions))
       (exception (assoc-get-default pitches exceptions #f))
       (prefixes '())
       (suffixes '())
       (add-steps '())
       (main-name #f)
       (bass-note #f)
       (alterations '())
       )

    (if
     exception
     (make-line-markup
      (list (name-root root) exception))
     
     (begin				; no exception.
       
       ; handle sus4 and sus2 suffix: if there is a 3 together with
       ; sus2 or sus4, then we explicitly say  add3.
       (map
	(lambda (j)
	  (if (get-step j pitches)
	      (begin
		(if (get-step 3 pitches)
		    (begin
		      (set! add-steps (cons (get-step 3 pitches) add-steps))
		      (set! pitches (remove-step 3 pitches))
		      ))
		(set! suffixes  (cons (get-step j pitches) suffixes))
		)
	      )
	  ) '(2 4) )

       ;; do minor-3rd modifier.
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

	 (if (ly:pitch? inversion)
	     (set! bass-note inversion)
	     )
	 
	 (if (ly:pitch? bass)
	     (set! bass-note bass)
	     )

	 ;; chords with natural (5 7 9 11 13) or leading subsequence.
	 ;; etc. are named by the top pitch, without any further
	 ;; alterations.
	 (if (and
	      (ly:pitch? main-name)
	      (= 7 (pitch-step main-name))
	      (is-natural-alteration? main-name)
	      (pair? (remove-uptil-step 7 alterations))
	      (reduce (lambda (x y) (and x y))
		      (map is-natural-alteration? alterations)))
	     (begin
	       (set! main-name (tail alterations))
	       (set! alterations '())
	       ))
	 
	 (ignatzek-format-chord-name root prefixes main-name alterations add-steps suffixes bass-note)
	 )
       ))))

