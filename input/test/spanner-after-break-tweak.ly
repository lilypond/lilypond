#(ly:set-option 'old-relative)
\version "1.9.1"

\header { texidoc = "@cindex Spanner after break

To selectively tweak spanners after the linebreaking
process, Scheme code must be used.  In this simple example, the tie
after the line break is moved around. "

      }

#(define (my-callback grob)
  (let* (
      (o (ly:get-original grob))
      (b (if (ly:grob? o) (ly:get-broken-into o) '() ))
      )

    ;; Call the equivalent of Tie::after_line_breaking
    ;; if you're using this for other grob-types.
    
    (if (and  (>= (length b) 2) (eq? (car (last-pair b)) grob))
	(ly:set-grob-property! grob 'extra-offset '(-2 . -1))
	)
  ))

#(debug-enable 'backtrace)

\score {\notes \relative c'' { 
    \property Voice.Tie \override #'after-line-breaking-callback =
       #my-callback
    c1 ~ \break c2 ~ c
}
    \paper { raggedright = ##t } 
    }
