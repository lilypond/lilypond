
\version "2.3.17"
\header {
    texidoc = "Instrument names are also printed on  partial starting measures."
}

\score {
   \relative c'' { \set Staff.instrument = "foo" \partial 4 c4 c1 }
  \paper { raggedright = ##t }
}

