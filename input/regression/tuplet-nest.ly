
\version "2.3.17"


\header {

    texidoc=" Nested tuplets can be printed, using a manual hack to
  move the outer bracket up."

}

#(define (make-text-checker-once text)
  (lambda (grob) (and text-checker-once
		      (if (equal? text (ly:grob-property grob 'text))
			  (begin
			    (set! text-checker-once #f) #t)
			  #f))))

#(define text-checker-once #t)

\score {
    \relative c'' {

    \set tupletNumberFormatFunction = #fraction-tuplet-formatter

    \applyoutput #(lambda (gr org cur)
		   (if (equal? (ly:grob-property gr 'text) "6:4")
		    (set! (ly:grob-property gr 'extra-offset) '(0 . 1.5))))
    \times 4/6 {
      \times 2/3 {
        a a a
      }
      \times 3/5 {
        a a a a a
      }
    }
  }
  \paper { raggedright = ##t}
}

