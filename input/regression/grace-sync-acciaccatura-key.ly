\version "2.27.5"

\header {
  texidoc = "
An @code{\\acciaccatura} in one staff must not desynchronize a key
signature change in a staff without grace note."
}

\layout { ragged-right = ##t }

<<
  \new Staff { c''2 \key g \major \acciaccatura d''8  c''2 }
  \new Staff { c'2 \key g \major c'2 }
>>
