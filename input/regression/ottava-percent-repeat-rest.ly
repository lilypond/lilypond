\version "2.25.26"

\header {
  texidoc = "Ottava brackets include single-percent repeat measures.  In this
test, the percent repeat is followed by a rest.  Double bar lines show where the
ottavation is canceled."
}

#(ly:set-option 'warning-as-error #t)

\fixed c'' {
  \ottava 1
  \repeat percent 2 { r4 d e r }
  \ottava 0 \bar "||"
  r4
}

\fixed c'' {
  \ottava 1
  \repeat percent 2 { r4 d e r }
  r4
  \ottava 0 \bar "||"
}
