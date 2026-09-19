\version "2.27.5"

\header {
  texidoc = "
A @code{\\section} bar line must be printed at the same place in all
staves, even if only one staff has a grace note after it."
}

\layout { ragged-right = ##t }

<<
  \new Staff { e''4 \section \grace c''16 d''2. }
  \new Staff { c''4 \section d''2. }
>>
