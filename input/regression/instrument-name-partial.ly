
\version "2.19.21"
\header {
  texidoc = "Instrument names are also printed on  partial starting measures."
}

\layout { ragged-right = ##t }



\relative { \set Staff.instrumentName = "foo" \partial 4 c''4 c1 }



