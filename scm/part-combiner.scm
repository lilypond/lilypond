

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; part-combiner.

(use-modules (oop goops))

(define-class <Voice-state> ()
  (event-list #:init-value '() #:accessor events #:init-keyword #:events)
  (when-moment #:accessor when #:init-keyword #:when)
  (split-idx #:accessor split-idx )
  (spanner-state #:init-value '() #:accessor span-state)
  )
  


(define-class <Split-state> ()
  (configuration #:init-value '() #:accessor configuration)
  (when-moment #:accessor when #:init-keyword #:when)
  (is #:init-keyword #:indexes #:accessor indexes)
  (synced  #:init-keyword #:synced #:init-value  #f #:getter synced?)
  )

(define-method (write (x <Voice-state> ) file)
  (display (when x) file)
  (display " evs = " file)
  (display (events x) file)
  (display " active = " file)
  (display (span-state x) file)
  (display "\n" file)
  )

(define-method (write (x <Split-state> ) f)
  (display (when x) f)
  (display " = " f)
  (display (configuration x) f)
  (if (synced? x)
      (display " synced "))
  (display "\n" f)
  )


(define (make-voice-states evl)
  (list->vector
  (map
   (lambda (v)
     (make <Voice-state>
       #:when (car v)
       #:events (map car (cdr v))
       ))
     evl)))

(define (moment-min a b)
  (if (ly:moment<? a b) a b))

(define (make-split-state vs1 vs2)
  "Merge lists VS1 and VS2, containing Voice-state objects into vector
of Split-state objects, crosslinking the Split-state vector and
Voice-state objects
"
  
  (define (helper ss-idx ss-list idx1 idx2)
    (let*
	((m1 (if (< idx1 (vector-length vs1)) (when (vector-ref vs1 idx1)) #f) )
	 (m2 (if (< idx2 (vector-length vs2)) (when (vector-ref vs2 idx2)) #f) )
	 (min (cond ((and m1 m2) (moment-min m1 m2))
		    (m1 m1)
		    (m2 m2)
		    (else #f)
		    ))

	 (inc1 (if (and m1 (equal? min m1)) 1 0))
	 (inc2 (if (and m2 (equal? min m2)) 1 0))
	 (ss-object
	  (if min
	      (make <Split-state>
		#:when min
		#:indexes (cons idx1 idx2)
		#:synced (= inc1 inc2)
		) #f))
	 )
      (if m1
	  (set! (split-idx (vector-ref vs1 idx1)) ss-idx))
      (if m2
	  (set! (split-idx (vector-ref vs2 idx2)) ss-idx))
      
      (if min
	  (helper (1+ ss-idx)
		  (cons ss-object ss-list)
		  (+ idx1 inc1)
		  (+ idx2 inc2))
	  ss-list
	  )
      ))

    (list->vector
     (reverse!
      (helper 0 '() 0  0) '()))
    )
      


(define (analyse-spanner-states voice-state-vec)

  (define (helper index active)
    "Analyse EVS at INDEX, given state ACTIVE."
    
    (define (analyse-tie-start active ev)
      (if (equal? (ly:get-mus-property ev 'name) 'TieEvent)
	  (acons 'tie index active)
	  active
	  ))
    
    (define (analyse-tie-end active ev)
      (if (equal? (ly:get-mus-property ev 'name) 'NoteEvent)
	  (assoc-remove! active 'tie)
	  active) )
    
    (define (active<? a b)
      (cond
       ((symbol<? (car a) (car b)) #t)
       ((symbol<? (car b) (car b)) #f)
       (else
	(< (cdr a) (cdr b)))
       ))
    
    (define (analyse-span-event active ev)
      (let*
	  ((name (ly:get-mus-property ev 'name))
	   (key (cond
		 ((equal? name 'SlurEvent) 'slur)
		 ((equal? name 'PhrasingSlurEvent) 'tie)
		 ((equal? name 'BeamEvent) 'beam)
		 ((equal? name 'CrescendoEvent) 'cresc)
		 ((equal? name 'DecrescendoEvent) 'decr)
		 (else #f)) )
	   (sp (ly:get-mus-property ev 'span-direction))
	   )

	(if (and (symbol? key) (ly:dir? sp))
	    (if (= sp STOP)
		(assoc-remove! active key)
		(acons key index active))
	    active)
	))

    (define (analyse-events active evs)
      "Run all analyzers on ACTIVE and EVS"

      (define (run-analyzer analyzer active evs)
	(if (pair? evs)
	    (run-analyzer analyzer (analyzer active (car evs)) (cdr evs))
	    active
	    ))

      (sort

       ;; todo: use fold or somesuch.
       (run-analyzer
	analyse-span-event
	(run-analyzer
	 analyse-tie-start
	 (run-analyzer analyse-tie-end active evs) evs) evs)
       
       active<?))

    ;; must copy, since we use assoc-remove!
    (if (< index (vector-length voice-state-vec))
	(begin
	  (set! active (analyse-events active (events (vector-ref voice-state-vec index))))
	  (set! (span-state (vector-ref voice-state-vec index))
		(list-copy active))

	  (helper (1+ index) active)))
    )


  (helper 0 '())
  
  )


	
(define noticed '())
(define part-combine-listener '())
(define-public (set-part-combine-listener x)
  (set! part-combine-listener x))

(define-public (notice-the-events-for-pc context lst)
  (set! noticed (acons (ly:context-id context) lst noticed)))

(define-public (make-new-part-combine-music music-list)
  (let*
     ((m (make-music-by-name 'NewPartCombineMusic))
      (m1 (context-spec-music (car music-list) 'Voice "one"))
      (m2 (context-spec-music (cadr music-list) 'Voice "two"))
      (props '((denies Thread)
	       (consists Rest_engraver)
	       (consists Note_heads_engraver)
	       )))
    
    (ly:set-mus-property! m 'elements (list m1 m2))
    (ly:set-mus-property! m1 'property-operations props)
    (ly:set-mus-property! m2 'property-operations props)
    (ly:run-translator m2 part-combine-listener)
    (ly:run-translator m1 part-combine-listener)
    (ly:set-mus-property! m 'split-list
			 (determine-split-list (reverse (cdr (assoc "one" noticed)))
					       (reverse (cdr (assoc "two" noticed)))))
    (set! noticed '())
    
    m))


    
    



;;
;; todo: this function is rather too hairy and too long.
;;
(define-public (determine-split-list evl1 evl2)
  "EVL1 and EVL2 should be ascending"

  (let*
      ((pc-debug #f)
       (chord-threshold 8)
       (voice-state-vec1 (make-voice-states evl1))
       (voice-state-vec2 (make-voice-states evl2))
       (result (make-split-state voice-state-vec1 voice-state-vec2))
       )


  (define (analyse-time-step ri)
    (define (put x . index)
      "Put the result to X, starting from INDEX backwards.

Only set if not set previously.
"
      
      (let
	  ((i (if (pair? index) (car index) ri)))

	(if (and (<= 0 i)
		 (not (symbol? (configuration (vector-ref result i)))))
	    (begin
	      (set! (configuration (vector-ref result i)) x)
	      (put x (1- i))
	    ))
	))

    (define (get-note-evs vs)
      (define (f? x)
	(equal? (ly:get-mus-property  x 'name) 'NoteEvent))
      (filter f? (events vs)))
    
    (define (copy-state-from state-vec vs)
      (define (copy-one-state key-idx)
	(let*
	    ((idx (cdr key-idx))
	     (start-vs (vector-ref state-vec idx))
	     (prev-ss (vector-ref result (split-idx start-vs)))
	     (prev (configuration prev-ss))
	     )
	  (if (symbol? prev)
	      (put prev))))
      
      (map copy-one-state (span-state vs))
      )

    (define (analyse-notes now-state) 
      (let*
	  (
	   (i1 (car (indexes now-state)))
	   (i2 (cdr (indexes now-state)))
	   (vs1 (vector-ref voice-state-vec1 i1))
	   (vs2 (vector-ref voice-state-vec2 i2))
	   
	   (notes1 (get-note-evs vs1))
	   (durs1 (sort (map (lambda (x) (ly:get-mus-property x 'duration)) notes1) ly:duration<?))
	   (pitches1 (sort
		      (map (lambda (x) (ly:get-mus-property x 'pitch)) notes1) ly:pitch<?))
	   (notes2 (get-note-evs vs2))
	   (durs2     (sort (map (lambda (x) (ly:get-mus-property x 'duration)) notes2) ly:duration<?))
	   (pitches2 (sort
		      (map (lambda (x) (ly:get-mus-property x 'pitch)) notes2) ly:pitch<?))
	   )
	
	(cond
	 ((> (length notes1) 1) (put 'apart))
	 ((> (length notes2) 1) (put 'apart))
	 ((not (= (length notes1) (length notes2)))
	  (put 'apart))
	 ((and
	   (= (length durs1) 1)
	   (= (length durs2) 1)
	   (not (equal? (car durs1) (car durs2))))

	  (put 'apart))
	 (else
	  (if (and (= (length pitches1) (length pitches2)))
	      (if (and (pair? pitches1)
		       (pair? pitches2)
		       (< chord-threshold (ly:pitch-steps
					   (ly:pitch-diff (car pitches1) (car pitches2)))))
		  (put 'apart)

		  ;; copy previous split state from spanner state
		  (begin
		    (if (> i1 0)
			(copy-state-from voice-state-vec1 (vector-ref voice-state-vec1 (1- i1))))
		    (if (> i2 0)
			(copy-state-from voice-state-vec2 (vector-ref voice-state-vec2 (1- i2))))
		    (if (and (null? (span-state vs1)) (null? (span-state vs2)))
			(put 'chords))
		    
		    ))))
	 )))
	 


    (if (< ri (vector-length result))
	(let*
	    ((now-state (vector-ref result ri))
	     (i1 (car (indexes now-state)))
	     (i2 (cdr (indexes now-state))))
	  
	  (cond
	   ((= i1 (vector-length voice-state-vec1)) (put 'apart))
	   ((= i2 (vector-length voice-state-vec2)) (put 'apart))
	   (else
	    (let*
		(
		 (vs1 (vector-ref voice-state-vec1 i1))
		 (vs2 (vector-ref voice-state-vec2 i2))
		 
		 (active1
		  (if (> i1 0)
		      (span-state (vector-ref voice-state-vec1 (1- i1)))
		      '()))
		 (active2
		  (if (> i2 0)
		      (span-state (vector-ref voice-state-vec2 (1- i2)))
		      '()))

		 (new-active1 (span-state vs1))
		 (new-active2 (span-state vs2))

		 )
	      (if
	       pc-debug
	       (display (list (when now-state) i1 i2 ri
				    active1 "->" new-active1
				    active2 "->" new-active2
				    "\n")))

	      
	      
	      (if (and (synced? now-state)
		       (equal? active1 active2)
		       (equal? new-active1 new-active2))

		  (analyse-notes now-state)

		  ;; active states different:
		  (put 'apart)
		  )
	      )

					; go to the next one, if it exists.
	    (analyse-time-step (1+ ri))
	    )))))
    
    
   (define (analyse-solo12 ri)
     (define (put x)
       (set-cdr! (vector-ref result ri) x) )
     
     (if (< ri (vector-length result))

       (let*
	  ((now (when result ri))
	   (m1 (when ev1 i1))
	   (m2 (when ev2 i2))
	   (notes1 (get-note-evs ev1
				 (if (ly:moment<?  now m1)
				     (1- i1) i1)))
	   
	   (durs1 (sort (map (lambda (x) (ly:get-mus-property x 'duration)) notes1) ly:duration<?))
	   (pitches1 (sort
		      (map (lambda (x) (ly:get-mus-property x 'pitch)) notes1) ly:pitch<?))

	   (notes2 (get-note-evs ev2
				 (if (ly:moment<? now m2)
				     (1- i2) i2)))
	   (n2 (length notes2))
	   (n1 (length notes1))
	   (durs2 (sort (map (lambda (x) (ly:get-mus-property x 'duration)) notes2) ly:duration<?))
	   (pitches2 (sort
		      (map (lambda (x) (ly:get-mus-property x 'pitch)) notes2) ly:pitch<?))
	   )

	(if pc-debug (display (list
			 "\n"
			 (when result ri) i1 "/" (vector-length ev1)
			      m1 ":" notes1
			      i2 "/" (vector-length ev2) m2 ":"
			      notes2
			      ri "/" (vector-length result)  " = "
			      (what  result ri)
			      "\n"
			      )))
    

	
	 (if (equal? (what result ri) 'apart)
	     (cond
	      ((and (= 0 n1)
		    (< 0 n2)
		    (equal? now m2)
		    )
	       (put 'solo2))
	      ((and (< 0 n1)
		    (= 0 n2)
		    (equal? now m1)
		    )
	       (put 'solo1))
	      ((and (= 0 n1)
		    (= 0 n2))
	       (put 'apart-silence))
	      ))

	 (if (and
	      (equal? (what result ri) 'chords)
	      (equal? pitches1 pitches2))
	     (put (if (pair? pitches2)
		      'unisono 'unisilence) ))
	 
	 (cond
	  ((ly:moment<? m1 m2)
	   (analyse-solo12 (1+ i1) i2 (1+ ri) ))
	  ((ly:moment<? m2 m1)
	   (analyse-solo12 i1 (1+ i2) (1+ ri) ))
	  (else
	   (analyse-solo12 (1+ i1) (1+ i2) (1+ ri)))
	  ))))


   (analyse-spanner-states voice-state-vec1)
   (analyse-spanner-states voice-state-vec2)
;  (display voice-state-vec1)
;   (display voice-state-vec2)
;   (display result)
     
   (analyse-time-step 0)
;   (analyse-solo12 0 0 0)
   (display result)
;   (if pc-debug (display result))

   (set! result    (map
		    (lambda (x) (cons (when x) (configuration x)))
		    (vector->list result)))

;   (if pc-debug (display result))
   result))
