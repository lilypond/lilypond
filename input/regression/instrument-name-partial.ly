
\version "2.6.0"
\header {
  texidoc = "Instrument names are also printed on  partial starting measures."
}


\relative c'' { \set Staff.instrument = "foo" \partial 4 c4 c1 }
\layout { raggedright = ##t }


