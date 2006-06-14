

#(set! toplevel-score-handler print-score-with-defaults)
#(set! toplevel-music-handler
  (lambda (p m)
   (if (not (eq? (ly:music-property m 'void) #t))
        (print-score-with-defaults
         p (scorify-music m p)))))

#(ly:set-option (quote no-point-and-click))
#(define inside-lilypond-book #t)
#(define version-seen #t)
