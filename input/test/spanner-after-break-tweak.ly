
\version "2.3.8"

\header { texidoc = "@cindex Spanner after break

In order to selectively change the properties of spanners after 
a line break, Scheme code must be used.  In thas manner, the tie
after the line break in this example is moved around. "

      }

#(define (my-callback grob)
  (let* (
      (o (ly:grob-original grob))
      (b (if (ly:grob? o) (ly:spanner-broken-into o) '() )))

    ;; Call the equivalent of Tie::after_line_breaking
    ;; if you're using this for other grob-types.
    
    (if (and  (>= (length b) 2) (eq? (car (last-pair b)) grob))
	(ly:grob-set-property! grob 'extra-offset '(-2 . 5))
	)))

\paper { raggedright = ##t } 

\relative c'' { 
    \override Tie  #'after-line-breaking-callback =
    #my-callback
    c1 ~ \break c2 ~ c
}
