
\version "2.1.26"

\header { texidoc = "@cindex Spanner after break

To selectively tweak spanners after the linebreaking
process, Scheme code must be used.  In this simple example, the tie
after the line break is moved around. "

      }

#(define (my-callback grob)
  (let* (
      (o (ly:grob-original grob))
      (b (if (ly:grob? o) (ly:spanner-broken-into o) '() ))
      )

    ;; Call the equivalent of Tie::after_line_breaking
    ;; if you're using this for other grob-types.
    
    (if (and  (>= (length b) 2) (eq? (car (last-pair b)) grob))
	(ly:grob-set-property! grob 'extra-offset '(-2 . -1))
	)
  ))

#(debug-enable 'backtrace)

\score {\notes \relative c'' { 
    \override Tie  #'after-line-breaking-callback =
       #my-callback
    c1 ~ \break c2 ~ c
}
    \paper { raggedright = ##t } 
    }
