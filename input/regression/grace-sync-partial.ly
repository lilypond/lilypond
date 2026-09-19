\version "2.27.5"

\header {
  texidoc = "
A grace note on the upbeat of one staff must not shift the time signature
or the partial measure handling of another staff."
}

\layout { ragged-right = ##t }

<<
  \new Staff { \time 3/4 \partial 4 \grace d''16 c''4 | b'2. }
  \new Staff { \time 3/4 \partial 4 c'4 | b2. }
>>
