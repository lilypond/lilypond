

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; autochange.scm - fairly related to part combining.

(define-public (make-autochange-music music)
  (define (generate-split-list change-moment prev-dir event-list acc)
    (if (null? event-list)
        acc
        (let* ((now-tun (caar event-list))
               (evs (map car (cdar event-list)))
               (now (car now-tun))
               (notes (filter (lambda (x)
                                (ly:in-event-class? x 'note-event))
                              evs))
               (pitch (if (pair? notes)
                          (ly:event-property (car notes) 'pitch)
                          #f))
               (dir (if pitch (sign (ly:pitch-steps pitch)) 0)))
          ;; tail recursive.
          (if (and (not (= dir 0))
                   (not (= dir prev-dir)))
              (generate-split-list #f
                                   dir
                                   (cdr event-list)
                                   (cons (cons

                                          (if change-moment
                                              change-moment
                                              now)
                                          (if (< dir 0) "down" "up")) acc))
              (generate-split-list
               (if pitch #f now)
               dir
               (cdr event-list) acc)))))

  (let* ((m (make-music 'AutoChangeMusic))
         (m1 (make-non-relative-music (context-spec-music music 'Voice "one")))
         (context-list (recording-group-emulate music
                                                (ly:parser-lookup 'partCombineListener)))
         (evs (car context-list))
         (rev (reverse! (cdar context-list)))
         (split (reverse! (generate-split-list
                           #f
                           0
                           rev
                           '())
                          '())))
    (set! (ly:music-property m 'element) music)
    (set! (ly:music-property m 'split-list) split)
    m))
