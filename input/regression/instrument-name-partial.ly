
\version "2.11.51"
\header {
  texidoc = "Instrument names are also printed on  partial starting measures."
}

\layout { ragged-right = ##t }



\relative c'' { \set Staff.instrumentName = "foo" \partial 4 c4 c1 }



