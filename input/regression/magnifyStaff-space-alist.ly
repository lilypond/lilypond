\version "2.19.22"

\header {
  texidoc = "@code{space-alist} values should be scaled along
with notation size when using the @code{\magnifyStaff} command."
}

\paper {
  indent = 0
  ragged-right = ##t
  system-system-spacing = #'((padding . 3))
  score-markup-spacing = #'((padding . 6))
}

example =
#(define-music-function (mag) (positive?)
   #{
     \new Staff \with {
       \magnifyStaff #mag
       \consists "Custos_engraver"
       \override Custos.style = #'mensural
     } \new Voice \with {
       \consists "Ambitus_engraver"
     } {
       \omit Score.BarNumber
       \key d \major
       d''2 \breathe d'' |
       \break
       g'1 |
       \clef treble
       \key c \major
       g'1 |
     }
   #})

\markup "0.50:" \example 0.50
\markup "1.00:" \example 1.00
\markup "2.00:" \example 2.00
