
\version "2.3.1"
\header {
  texidoc="By a manual hack for nested tuplets, an outer tuplet can be moved up."
}

#(define (make-text-checker-once text)
  (lambda (grob) (and text-checker-once
		      (if (equal? text (ly:grob-property grob 'text))
			  (begin
			    (set! text-checker-once #f) #t)
			  #f))))

#(define text-checker-once #t)

\score {
  \notes\relative c'' {

    \set tupletNumberFormatFunction = #fraction-tuplet-formatter

    \applyoutput #(outputproperty-compatibility (make-text-checker-once "2:3")
		   'extra-offset '(0 . 1.5))
    \times 2/3 {
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

