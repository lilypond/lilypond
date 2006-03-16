
\version "2.7.39"
\header {
  texidoc = "Instrument names are also printed on  partial starting measures."
}

\layout { ragged-right = ##t }



\relative c'' { \set Staff.instrument = "foo" \partial 4 c4 c1 }



