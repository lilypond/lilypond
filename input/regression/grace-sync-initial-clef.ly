\version "2.27.5"

\header {
  texidoc = "
A grace note at the very start of one staff must not disturb the initial
clef and key signature of another staff that has no grace note.  Both
staves should start with their clef and key signature in the usual place."
}

\layout { ragged-right = ##t }

<<
  \new Staff { \key d \major \grace c''8 c''1 }
  \new Staff { \clef bass \key d \major c1 }
>>
