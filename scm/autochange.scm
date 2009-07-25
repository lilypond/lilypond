

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; autochange.scm - fairly related to part combining.

(define-public (make-autochange-music parser music)
  (define (generate-split-list change-moment event-list acc)
    (if (null? event-list)
	acc
	(let* ((now-tun (caar event-list))
	       (evs (map car (cdar event-list)))
	       (now (car now-tun))
	       (notes (filter (lambda (x)
				(equal? (ly:event-property  x 'class) 'note-event))
			      evs))
	       (pitch (if (pair? notes)
			  (ly:event-property (car notes) 'pitch)
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
  
  (let* ((m (make-music 'AutoChangeMusic))
	(m1 (make-non-relative-music (context-spec-music music 'Voice "one")))
	 (context-list (recording-group-emulate music
						(ly:parser-lookup parser 'partCombineListener)))
	 (evs (car context-list))
         (rev (reverse! (cdar context-list)))
	 (split (reverse! (generate-split-list
			   #f
			   rev
			   '())
			  '())))
    (set! (ly:music-property m 'element) music)
    (set! (ly:music-property m 'split-list) split)
    m))
