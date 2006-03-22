
\version "2.8.0"
\header {
  texidoc = "Instrument names are also printed on  partial starting measures."
}

\layout { ragged-right = ##t }



\relative c'' { \set Staff.instrument = "foo" \partial 4 c4 c1 }



