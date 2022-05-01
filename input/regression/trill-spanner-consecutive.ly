\version "2.23.9"

\header {
  texidoc = "Consecutive trill spanners never overlap."
}

test =
#(define-music-function (inc) (number?)
   #{
     \override Score.SpacingSpanner.spacing-increment = #inc
     c'2\startTrillSpan c'2\startTrillSpan
   #})

\test 2.0
\test 2.1
\test 2.2
\test 2.3
