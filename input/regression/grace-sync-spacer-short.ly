\version "2.27.5"

\header {
  texidoc = "
An invisible @code{\\grace s16} that is shorter than the real grace note
in the other staff should still leave both staves with correctly placed
initial clefs."
}

\layout { ragged-right = ##t }

<<
  \new Staff { \grace c''8 c''1 }
  \new Staff { \clef bass \grace s16 c1 }
>>
