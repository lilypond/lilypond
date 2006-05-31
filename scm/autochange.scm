

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; autochange - fairly related to part combining.

(define-public (make-autochange-music music)
  (define (generate-split-list change-moment event-list acc)
    (if (null? event-list)
	acc
	(let* ((now-tun (caar event-list))
	       (evs (map car (cdar event-list)))
	       (now (car now-tun))
	       (notes (filter (lambda (x)
				(equal? (ly:music-property  x 'name) 'NoteEvent))
			      evs))
	       (pitch (if (pair? notes)
			  (ly:music-property (car notes) 'pitch)
			  #f)))
	  ;; tail recursive.
	  (if (and pitch (not (= (ly:pitch-steps pitch) 0)))
	      (generate-split-list #f
				   (cdr event-list)
				   (cons (cons

					  (if change-moment
					      change-moment
					      now)
					  (sign (ly:pitch-steps pitch))) acc))
	      (generate-split-list
	       (if pitch #f now)
	       (cdr event-list) acc)))))
  
  (set! noticed '())
  (let* ((m (make-music 'AutoChangeMusic))
	 (context (ly:run-translator (make-non-relative-music music) part-combine-listener))
	 (evs (last-pair noticed))
	 (split (reverse! (generate-split-list
			   #f
			   (if (pair? evs)
			       (reverse! (cdar evs) '()) '())
			   '())
			  '())))
    (set! (ly:music-property m 'element) music)
    (set! (ly:music-property m 'split-list) split)
    (set! noticed '())
    m))
