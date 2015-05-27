\version "2.19.22"

\header {
  texidoc = "Bar line thickness and spacing should be scaled along
with notation size when using the @code{\magnifyStaff} command."
}

\paper {
  score-system-spacing = #'((padding . 4))
}

example =
#(define-music-function (mag) (positive?)
   #{
     \new Staff \with {
       \magnifyStaff #mag
       instrumentName = \markup {
         \fontsize #(+ 3 (- (magnification->font-size mag)))
         #(format #f "~,2f" mag)
       }
     } {
       \omit Staff.Clef
       \omit Staff.TimeSignature
       s4 \bar "|"
       s4 \bar ":|.|:"
       s4 \bar ":|.S.|:"
       s4 \bar "|."
     }
   #})

\example 0.50
\example 1.00
\example 2.00
