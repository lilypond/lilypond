\version "2.25.8"

\header {
  doctitle = "Beam subdivision of a pentadecuplet (denominator 15)"

  texidoc = "The default subdivision of a pentadecuplet should be none
as the hamming weight of the denominator is more than@tie{}1."
}


\paper {
  indent = 0
  ragged-right = ##t
}

\relative c'' {
  \time 1/4
  \set subdivideBeams = ##t
  \omit Staff.Clef

  \tuplet 15/8 { \repeat unfold 15 c32 }
}
