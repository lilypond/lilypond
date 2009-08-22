;;;; part-combiner.scm -- Part combining, staff changes.
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;;
;;;; (c) 2004--2009	Han-Wen Nienhuys <hanwen@xs4all.nl>

;; todo: figure out how to make module,
;; without breaking nested ly scopes

(define-class <Voice-state> ()
  (event-list #:init-value '() #:accessor events #:init-keyword #:events)
  (when-moment #:accessor when #:init-keyword #:when)
  (tuning #:accessor tuning #:init-keyword #:tuning)
  (split-index #:accessor split-index)
  (vector-index)
  (state-vector)
  ;;;
  ;; spanner-state is an alist
  ;; of (SYMBOL . RESULT-INDEX), which indicates where
  ;; said spanner was started.
  (spanner-state #:init-value '() #:accessor span-state))

(define-method (write (x <Voice-state> ) file)
  (display (when x) file)
  (display " evs = " file)
  (display (events x) file)
  (display " active = " file)
  (display (span-state x) file)
  (display "\n" file))

(define-method (note-events (vs <Voice-state>))
  (define (f? x)
    (equal? (ly:event-property	x 'class) 'note-event))
  (filter f? (events vs)))

(define-method (previous-voice-state (vs <Voice-state>))
  (let ((i (slot-ref vs 'vector-index))
	(v (slot-ref vs 'state-vector)))
    (if (< 0 i)
	(vector-ref v (1- i))
	#f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-class <Split-state> ()
  (configuration #:init-value '() #:accessor configuration)
  (when-moment #:accessor when #:init-keyword #:when)
  ;; voice-states are states starting with the Split-state or later
  ;;
  (is #:init-keyword #:voice-states #:accessor voice-states)
  (synced  #:init-keyword #:synced #:init-value	 #f #:getter synced?))


(define-method (write (x <Split-state> ) f)
  (display (when x) f)
  (display " = " f)
  (display (configuration x) f)
  (if (synced? x)
      (display " synced "))
  (display "\n" f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (previous-span-state vs)
  (let ((p (previous-voice-state vs)))
    (if p (span-state p) '())))

(define (make-voice-states evl)
  (let ((vec (list->vector (map (lambda (v)
				  (make <Voice-state>
				    #:when (caar v)
				    #:tuning (cdar v)
				    #:events (map car (cdr v))))
				evl))))
    (do ((i 0 (1+ i)))
	((= i (vector-length vec)) vec)
      (slot-set! (vector-ref vec i) 'vector-index i)
      (slot-set! (vector-ref vec i) 'state-vector vec))))

(define (make-split-state vs1 vs2)
  "Merge lists VS1 and VS2, containing Voice-state objects into vector
of Split-state objects, crosslinking the Split-state vector and
Voice-state objects
"
  (define (helper ss-idx ss-list idx1 idx2)
    (let* ((state1 (if (< idx1 (vector-length vs1)) (vector-ref vs1 idx1) #f))
	   (state2 (if (< idx2 (vector-length vs2)) (vector-ref vs2 idx2) #f))
	   (min (cond ((and state1 state2) (moment-min (when state1) (when state2)))
		      (state1 (when state1))
		      (state2 (when state2))
		      (else #f)))
	   (inc1 (if (and state1 (equal? min (when state1))) 1 0))
	   (inc2 (if (and state2 (equal? min (when state2))) 1 0))
	   (ss-object (if min
			  (make <Split-state>
			    #:when min
			    #:voice-states (cons state1 state2)
			    #:synced (= inc1 inc2))
			  #f)))
      (if state1
	  (set! (split-index state1) ss-idx))
      (if state2
	  (set! (split-index state2) ss-idx))
      (if min
	  (helper (1+ ss-idx)
		  (cons ss-object ss-list)
		  (+ idx1 inc1)
		  (+ idx2 inc2))
	  ss-list)))
  (list->vector (reverse! (helper 0 '() 0  0) '())))

(define (analyse-spanner-states voice-state-vec)

  (define (helper index active)
    "Analyse EVS at INDEX, given state ACTIVE."

    (define (analyse-tie-start active ev)
      (if (equal? (ly:event-property ev 'class) 'tie-event)
	  (acons 'tie (split-index (vector-ref voice-state-vec index))
		 active)
	  active))

    (define (analyse-tie-end active ev)
      (if (equal? (ly:event-property ev 'class) 'note-event)
	  (assoc-remove! active 'tie)
	  active))

    (define (analyse-absdyn-end active ev)
      (if (or (equal? (ly:event-property ev 'class) 'absolute-dynamic-event)
	      (and (equal? (ly:event-property ev 'class) 'crescendo-event)
		   (equal? STOP (ly:event-property ev 'span-direction))))
	  (assoc-remove! (assoc-remove! active 'cresc) 'decr)
	  active))

    (define (active<? a b)
      (cond ((symbol<? (car a) (car b)) #t)
	    ((symbol<? (car b) (car b)) #f)
	    (else (< (cdr a) (cdr b)))))

    (define (analyse-span-event active ev)
      (let* ((name (ly:event-property ev 'class))
	     (key (cond ((equal? name 'slur-event) 'slur)
			((equal? name 'phrasing-slur-event) 'tie)
			((equal? name 'beam-event) 'beam)
			((equal? name 'crescendo-event) 'cresc)
			((equal? name 'decrescendo-event) 'decr)
			(else #f)))
	     (sp (ly:event-property ev 'span-direction)))
	(if (and (symbol? key) (ly:dir? sp))
	    (if (= sp STOP)
		(assoc-remove! active key)
		(acons key
		       (split-index (vector-ref voice-state-vec index))
		       active))
	    active)))

    (define (analyse-events active evs)
      "Run all analyzers on ACTIVE and EVS"
      (define (run-analyzer analyzer active evs)
	(if (pair? evs)
	    (run-analyzer analyzer (analyzer active (car evs)) (cdr evs))
	    active))
      (define (run-analyzers analyzers active evs)
	(if (pair? analyzers)
	    (run-analyzers (cdr analyzers)
			   (run-analyzer (car analyzers) active evs)
			   evs)
	    active))
      (sort ;; todo: use fold or somesuch.
       (run-analyzers (list analyse-absdyn-end analyse-span-event
			    ;; note: tie-start/span comes after tie-end/absdyn.
			    analyse-tie-end analyse-tie-start)
		      active evs)
       active<?))

    ;; must copy, since we use assoc-remove!
    (if (< index (vector-length voice-state-vec))
	(begin
	  (set! active (analyse-events active (events (vector-ref voice-state-vec index))))
	  (set! (span-state (vector-ref voice-state-vec index))
		(list-copy active))
	  (helper (1+ index) active))))

  (helper 0 '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-public (recording-group-emulate music odef)
  "Interprets music according to odef, but stores all events in a chronological
list, similar to the Recording_group_engraver in 2.8 and earlier"
  (let*
      ((context-list '())
       (now-mom (ly:make-moment 0 0))
       (global (ly:make-global-context odef))
       (mom-listener (ly:make-listener
		      (lambda (tev)
			(set! now-mom (ly:event-property tev 'moment)))))
       (new-context-listener
	(ly:make-listener
	 (lambda (sev)
	     (let*
		 ((child (ly:event-property sev 'context))
		  (this-moment-list
		   (cons (ly:context-id child) '()))
		  (dummy
		   (set! context-list (cons this-moment-list context-list)))
		  (acc '())
		  (accumulate-event-listener
		   (ly:make-listener (lambda (ev)
				       (set! acc (cons (cons ev #t) acc)))))
		  (save-acc-listener (ly:make-listener (lambda (tev)
							 (if (pair? acc)
							     (let ((this-moment (cons (cons now-mom (ly:context-property child 'instrumentTransposition))
										      ;; The accumulate-event-listener above creates the list of events in reverse order,
										      ;; so we have to revert it to be in the original order again
										      (reverse acc))))
							       (set-cdr! this-moment-list (cons this-moment (cdr this-moment-list)))
							       (set! acc '())))))))
	       (ly:add-listener accumulate-event-listener (ly:context-event-source child) 'StreamEvent)
	       (ly:add-listener save-acc-listener (ly:context-event-source global) 'OneTimeStep))))))
    (ly:add-listener new-context-listener (ly:context-events-below global) 'AnnounceNewContext)
    (ly:add-listener mom-listener (ly:context-event-source global) 'Prepare)
    (ly:interpret-music-expression (make-non-relative-music music) global)
    context-list))

(define-public (make-part-combine-music parser music-list)
  (let* ((m (make-music 'PartCombineMusic))
	 (m1 (make-non-relative-music (context-spec-music (first music-list) 'Voice "one")))
	 (m2  (make-non-relative-music  (context-spec-music (second music-list) 'Voice "two")))
	 (listener (ly:parser-lookup parser 'partCombineListener))
	 (evs2 (recording-group-emulate m2 listener))
	 (evs1 (recording-group-emulate m1 listener)))

    (set! (ly:music-property m 'elements) (list m1 m2))
    (set! (ly:music-property m 'split-list)
	  (if (and (assoc "one" evs1) (assoc "two" evs2))
	      (determine-split-list (reverse! (assoc-get "one" evs1) '())
				    (reverse! (assoc-get "two" evs2) '()))
	      '()))
    m))

(define-public (determine-split-list evl1 evl2)
  "EVL1 and EVL2 should be ascending"
  (let* ((pc-debug #f)
	 (chord-threshold 8)
	 (voice-state-vec1 (make-voice-states evl1))
	 (voice-state-vec2 (make-voice-states evl2))
	 (result (make-split-state voice-state-vec1 voice-state-vec2)))

    (define (analyse-time-step result-idx)
      (define (put x . index)
	"Put the result to X, starting from INDEX backwards.

Only set if not set previously.
"
	(let ((i (if (pair? index) (car index) result-idx)))
	  (if (and (<= 0 i)
		   (not (symbol? (configuration (vector-ref result i)))))
	      (begin
		(set! (configuration (vector-ref result i)) x)
		(put x (1- i))))))

      (define (copy-state-from state-vec vs)
	(define (copy-one-state key-idx)
	  (let* ((idx (cdr key-idx))
		 (prev-ss (vector-ref result idx))
		 (prev (configuration prev-ss)))
	    (if (symbol? prev)
		(put prev))))
	(map copy-one-state (span-state vs)))

      (define (analyse-notes now-state)
	(let* ((vs1 (car (voice-states now-state)))
	       (vs2 (cdr (voice-states now-state)))
	       (notes1 (note-events vs1))
	       (durs1 (sort (map (lambda (x) (ly:event-property x 'duration))
				 notes1)
			    ly:duration<?))
	       (pitches1 (sort (map (lambda (x) (ly:event-property x 'pitch))
				    notes1)
			       ly:pitch<?))
	       (notes2 (note-events vs2))
	       (durs2 (sort (map (lambda (x) (ly:event-property x 'duration))
				 notes2)
			    ly:duration<?))
	       (pitches2 (sort (map (lambda (x) (ly:event-property x 'pitch))
				    notes2)
			       ly:pitch<?)))
	  (cond ((> (length notes1) 1) (put 'apart))
		((> (length notes2) 1) (put 'apart))
		((= 1 (+ (length notes2) (length notes1))) (put 'apart))
		((and (= (length durs1) 1)
		      (= (length durs2) 1)
		      (not (equal? (car durs1) (car durs2))))
		 (put 'apart))
		(else
		 (if (and (= (length pitches1) (length pitches2)))
		     (if (and (pair? pitches1)
			      (pair? pitches2)
			      (or
			       (< chord-threshold (ly:pitch-steps
						   (ly:pitch-diff (car pitches1)
								  (car pitches2))))

			       ;; voice crossings:
			       (> 0 (ly:pitch-steps (ly:pitch-diff (car pitches1)
								   (car pitches2))))
			       ))
			 (put 'apart)
			 ;; copy previous split state from spanner state
			 (begin
			   (if (previous-voice-state vs1)
			       (copy-state-from voice-state-vec1
						(previous-voice-state vs1)))
			   (if (previous-voice-state vs2)
			       (copy-state-from voice-state-vec2
						(previous-voice-state vs2)))
			   (if (and (null? (span-state vs1)) (null? (span-state vs2)))
			       (put 'chords)))))))))

      (if (< result-idx (vector-length result))
	  (let* ((now-state (vector-ref result result-idx))
		 (vs1 (car (voice-states now-state)))
		 (vs2 (cdr (voice-states now-state))))

	    (cond ((not vs1) (put 'apart))
		  ((not vs2) (put 'apart))
		  (else
		   (let ((active1 (previous-span-state vs1))
			 (active2 (previous-span-state vs2))
			 (new-active1 (span-state vs1))
			 (new-active2 (span-state vs2)))
		     (if #f ; debug
			 (display (list (when now-state) result-idx
					active1 "->" new-active1
					active2 "->" new-active2
					"\n")))
		     (if (and (synced? now-state)
			      (equal? active1 active2)
			      (equal? new-active1 new-active2))
			 (analyse-notes now-state)

			 ;; active states different:
			 (put 'apart)))

		   ;; go to the next one, if it exists.
		   (analyse-time-step (1+ result-idx)))))))

    (define (analyse-a2 result-idx)
      (if (< result-idx (vector-length result))
	  (let* ((now-state (vector-ref result result-idx))
		 (vs1 (car (voice-states now-state)))
		 (vs2 (cdr (voice-states now-state))))
	    (if (and (equal? (configuration now-state) 'chords)
		     vs1 vs2)
		(let ((notes1 (note-events vs1))
		      (notes2 (note-events vs2)))
		  (cond ((and (= 1 (length notes1))
			      (= 1 (length notes2))
			      (equal? (ly:event-property (car notes1) 'pitch)
				      (ly:event-property (car notes2) 'pitch)))
			 (set! (configuration now-state) 'unisono))
			((and (= 0 (length notes1))
			      (= 0 (length notes2)))
			 (set! (configuration now-state) 'unisilence)))))
	    (analyse-a2 (1+ result-idx)))))

    (define (analyse-solo12 result-idx)

      (define (previous-config vs)
	(let* ((pvs (previous-voice-state vs))
	       (spi (if pvs (split-index pvs) #f))
	       (prev-split (if spi (vector-ref result spi) #f)))
	  (if prev-split
	      (configuration prev-split)
	      'apart)))

      (define (put-range x a b)
	;; (display (list "put range "	x a b "\n"))
	(do ((i a (1+ i)))
	    ((> i b) b)
	  (set! (configuration (vector-ref result i)) x)))

      (define (put x)
	;; (display (list "putting "  x "\n"))
	(set! (configuration (vector-ref result result-idx)) x))

      (define (current-voice-state now-state voice-num)
	(define vs ((if (= 1 voice-num) car cdr)
		    (voice-states now-state)))
	(if (or (not vs) (equal? (when now-state) (when vs)))
	    vs
	    (previous-voice-state vs)))

      (define (try-solo type start-idx current-idx)
	"Find a maximum stretch that can be marked as solo. Only set
the mark when there are no spanners active.

      return next idx to analyse.
"
	(if (< current-idx (vector-length result))
	    (let* ((now-state (vector-ref result current-idx))
		   (solo-state (current-voice-state now-state (if (equal? type 'solo1) 1 2)))
		   (silent-state (current-voice-state now-state (if (equal? type 'solo1) 2 1)))
		   (silent-notes (if silent-state (note-events silent-state) '()))
		   (solo-notes (if solo-state (note-events solo-state) '())))
	      ;; (display (list "trying " type " at "  (when now-state) solo-state silent-state	 "\n"))
	      (cond ((not (equal? (configuration now-state) 'apart))
		     current-idx)
		    ((> (length silent-notes) 0) start-idx)
		    ((not solo-state)
		     (put-range type start-idx current-idx)
		     current-idx)
		    ((and
		      (null? (span-state solo-state)))

		     ;;
		     ;; This includes rests. This isn't a problem: long rests
		     ;; will be shared with the silent voice, and be marked
		     ;; as unisilence. Therefore, long rests won't
		     ;;	 accidentally be part of a solo.
		     ;;
		     (put-range type start-idx current-idx)
		     (try-solo type (1+ current-idx) (1+  current-idx)))
		    (else
		     (try-solo type start-idx (1+ current-idx)))))
	    ;; try-solo
	    start-idx))

      (define (analyse-moment result-idx)
	"Analyse 'apart starting at RESULT-IDX. Return next index. "
	(let* ((now-state (vector-ref result result-idx))
	       (vs1 (current-voice-state now-state 1))
	       (vs2 (current-voice-state now-state 2))
	       ;; (vs1 (car (voice-states now-state)))
	       ;; (vs2 (cdr (voice-states now-state)))
	       (notes1 (if vs1 (note-events vs1) '()))
	       (notes2 (if vs2 (note-events vs2) '()))
	       (n1 (length notes1))
	       (n2 (length notes2)))
	  ;; (display (list "analyzing step " result-idx "  moment " (when now-state) vs1 vs2  "\n"))
	  (max
	   ;; we should always increase.
	   (cond ((and (= n1 0) (= n2 0))
		  (put 'apart-silence)
		  (1+ result-idx))
		 ((and (= n2 0)
		       (equal? (when vs1) (when now-state))
		       (null? (previous-span-state vs1)))
		  (try-solo 'solo1 result-idx result-idx))
		 ((and (= n1 0)
		       (equal? (when vs2) (when now-state))
		       (null? (previous-span-state vs2)))
		  (try-solo 'solo2 result-idx result-idx))

		 (else (1+ result-idx)))
	   ;; analyse-moment
	   (1+ result-idx))))

      (if (< result-idx (vector-length result))
	  (if (equal? (configuration (vector-ref result result-idx)) 'apart)
	      (analyse-solo12 (analyse-moment result-idx))
	      (analyse-solo12 (1+ result-idx))))) ; analyse-solo12

    (analyse-spanner-states voice-state-vec1)
    (analyse-spanner-states voice-state-vec2)
    (if #f
	(begin
	  (display voice-state-vec1)
	  (display "***\n")
	  (display voice-state-vec2)
	  (display "***\n")
	  (display result)
	  (display "***\n")))
    (analyse-time-step 0)
    ;; (display result)
    (analyse-a2 0)
    ;;(display result)
    (analyse-solo12 0)
    ;; (display result)
    (set! result (map
		  (lambda (x) (cons (when x) (configuration x)))
		  (vector->list result)))
    (if #f ;; pc-debug
	 (display result))
    result))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (add-quotable parser name mus)
  (let* ((tab (eval 'musicQuotes (current-module)))
	 (context-list (recording-group-emulate (context-spec-music mus 'Voice)
						(ly:parser-lookup parser 'partCombineListener))))
    (if (pair? context-list)
	(hash-set! tab name
		   ;; cdr : skip name string
		   (list->vector (reverse! (cdar context-list)
					   '()))))))
