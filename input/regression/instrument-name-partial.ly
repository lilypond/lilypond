
\version "2.1.7"
\header {
    texidoc = "Instrument names are also printed on  partial starting measures."
}

\score {
  \notes \relative c'' { \property Staff.instrument = "foo" \partial 4 c4 c1 }
  \paper { raggedright = ##t }
}

