

(define (make-chord pitches bass duration)
  "Make EventChord with notes corresponding to PITCHES, BASS and DURATION. " 
  (define (make-note-ev pitch)
    (let*
	(
	 (ev   (make-music-by-name 'NoteEvent))
	 )

      (ly:set-mus-property! ev 'duration duration)
      (ly:set-mus-property! ev 'pitch pitch)
      ev      
      ))
  
  (let*
      (
       (nots (map make-note-ev pitches))
       (bass-note (if bass (make-note-ev bass) #f)) 
       )
    
    (if bass-note
	(begin
	  (ly:set-mus-property! bass-note 'bass #t)
	  (set! nots (cons bass-note nots))))

    (make-event-chord nots)
  ))


(define (aug-modifier root pitches)
  (set! pitches  (replace-step (ly:pitch-transpose (ly:make-pitch 0 4 1) root) pitches))
  (replace-step (ly:pitch-transpose (ly:make-pitch 0 2 0) root) pitches) 
  )


(define (minor-modifier root pitches)
  (replace-step (ly:pitch-transpose (ly:make-pitch 0 2 -1) root) pitches)
  )

(define (maj7-modifier root pitches)
  (set! pitches (remove-step 7 pitches))
  (cons  (ly:pitch-transpose (ly:make-pitch 0 6 0) root) pitches)
  )

(define (dim-modifier root pitches)
  (set! pitches (replace-step (ly:pitch-transpose (ly:make-pitch 0 2 -1) root) pitches))
  (set! pitches (replace-step (ly:pitch-transpose (ly:make-pitch 0 4 -1) root) pitches))
  (set! pitches (replace-step (ly:pitch-transpose (ly:make-pitch 0 6 -2) root) pitches))
  pitches
  )


(define (sus2-modifier root pitches)
  (set! pitches (remove-step (pitch-step (ly:pitch-transpose (ly:make-pitch 0 2 0) root)) pitches))
  (set! pitches (remove-step (pitch-step (ly:pitch-transpose (ly:make-pitch 0 3 0) root)) pitches))
  (cons (ly:pitch-transpose (ly:make-pitch 0 1 0) root) pitches)
  )

(define (sus4-modifier root pitches)
  (set! pitches (remove-step (pitch-step (ly:pitch-transpose (ly:make-pitch 0 2 0) root)) pitches))
  (set! pitches (remove-step (pitch-step (ly:pitch-transpose (ly:make-pitch 0 3 0) root)) pitches))
  (cons (ly:pitch-transpose (ly:make-pitch 0 3 0) root) pitches)
  )

(define-public default-chord-modifier-list
  `((m . ,minor-modifier)
    (min . ,minor-modifier)
    (aug . , aug-modifier)
    (dim . , dim-modifier)
    (maj . , maj7-modifier)
    (sus . , sus4-modifier)
    ))

(define (gobble-pitches lst)
  (if (null? lst)
      '()
      (if (ly:pitch? (car lst))
	  (gobble-pitches (cdr lst))
	  lst
	  )))


;; ? should remove 3 if sus2 or sus4 found? 
(define (add-pitches root pitches to-add)
  (if
   (or (null? to-add) (not (ly:pitch? (car to-add))))
   pitches
   (let*
       (
	(p (ly:pitch-transpose  (car to-add) root))
	(step (pitch-step p))
	)
     (if (get-step step pitches)
	 (set! pitches (remove-step step pitches)))
     (add-pitches root (cons p pitches) (cdr to-add)))))

(define (rm-pitches root pitches to-add)
  (if
   (or (null? to-add) (not (ly:pitch? (car to-add))))
   pitches
   (let*
       (
	(p (ly:pitch-transpose (car to-add) root))
	(step (pitch-step p))
	)
     (rm-pitches root (remove-step step pitches) (cdr to-add)))))


(define-public (construct-chord root duration modifications)
  (let*
      (
       (flat-mods (flatten-list modifications))
       (base-chord (list root
			 (ly:pitch-transpose (ly:make-pitch 0 2 0) root)
			 (ly:pitch-transpose (ly:make-pitch 0 4 0) root)))
       (complete-chord '())
       (bass #f)
       (inversion #f)
       )

    (define (process-inversion note-evs inversion)

      ;; TODO
      ;; Transpose the inversion down, and remember its original octave.
      note-evs
      )
    
    (define (interpret-chord root chord mods)
      "Walk MODS, and apply each mod to CHORD in turn.

Side-effect: set BASS and INVERSION in containing body
"
      ;; the recursion makes this into a loop. Perhaps its better to
      ;; to do the different types of modifiers in order, so that
      ;; addition _always_ precedes removal. 
      (if (null? mods)
	  chord
	  (let* (
		 (tag (car mods))
		 (tail (cdr mods))
		 )
	    (cond
	     ((procedure? tag)
	      (interpret-chord root 
			       (tag root chord)
			       tail))
	     ((equal? tag 'chord-colon)
	      (interpret-chord root
			       (add-pitches root chord tail)
			       (gobble-pitches tail)))
	     ((equal? tag 'chord-caret)
	      (interpret-chord root
			       (rm-pitches root chord tail)
			       (gobble-pitches tail)))
	     
	     ((equal? tag 'chord-slash)
	      (set! inversion (car tail))
	      (interpret-chord root
			       chord
			       (gobble-pitches tail)))
	     ((equal? tag 'chord-bass)
	      (set! bass (car tail)) 
	      (interpret-chord root
			       chord
			       (gobble-pitches tail)))

	     ;; ugh. Simply add isolated pitches. This will give
	     ;; unexpected results....
	     ((ly:pitch? tag)
	      (interpret-chord root
			       (add-pitches root chord tail)
			       (gobble-pitches tail)))
	     (else (scm-error 'chord-entry 'interpret-chord  "Unknown chord instructions ~S." (list mods) #f))
	     )
	    )
	  ))

    (write-me "*******\n" flat-mods)
    (write-me "pitches: " complete-chord)
    (write-me "bass: " bass)

    (set! complete-chord (interpret-chord root base-chord flat-mods))
    (set! complete-chord (sort complete-chord ly:pitch<?))
    
    ;; TODO: lower bass to be below chord.
    (process-inversion (make-chord complete-chord bass duration) inversion)
    
  ))
