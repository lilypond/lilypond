\version "1.7.3"
\header {

	 texidoc = "Broken spanners can be adjusted individually, but
this requires complicated scheme code.
  "

}

#(define (my-callback grob)
  (let* (
      (o (ly:get-original grob))
      (b (if (ly:grob? o) (ly:get-broken-into o) '() ))
      )

    ;; Call the equivalent of Tie::after_line_breaking
    ;; if you're using this for other grob-types.
    
    (if (and  (>= (length b) 2) (eq? (car (last-pair b)) grob))
	(ly:set-grob-property! grob 'extra-offset '(4 . -2))
	)
  ))

#(debug-enable 'backtrace)

\score {\notes { 
    \property Voice.Tie \override #'after-line-breaking-callback =
       #my-callback
    c1\break ~ c2 ~ c
}
    \paper { linewidth= 5.0 \cm }
    }
