\version "2.17.16"

\header {
  texidoc =
"The column of dots on a chord is limited to the height
of the chord plus @code{chord-dots-limit} staff-positions."
}

\layout{ ragged-right = ##t }

\new Staff \transpose c c' {
  \override Staff.DotColumn.chord-dots-limit = #1
  <<
    { <g a b c' d' e'>4. r8 <c' d' e' f' g' a' b'>4. r8 } \\
    { f4.. r16 <c d e f g a b>4.. r16}
  >>
}
