\version "2.27.5"

\header {
  texidoc = "
A key signature change written before a @code{\\grace} in one staff must
line up with the key signature change in a staff without grace note."
}

\layout { ragged-right = ##t }

<<
  \new Staff { c''1 \key d \major \grace e''8 d''1 }
  \new Staff { c'1 \key d \major d'1 }
>>
