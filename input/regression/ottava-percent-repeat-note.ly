\version "2.25.26"

\header {
  texidoc = "Ottava brackets include single-percent repeat measures.  In this
test, the percent repeat is followed by a note."
}

#(ly:set-option 'warning-as-error #t)

\fixed c'' {
  \ottava 1
  \repeat percent 2 { c4 d e f }
  g1
  \ottava 0
}

\fixed c'' {
  \ottava 1
  \repeat percent 2 { c4 d e f }
  \ottava 0
  g,1^\markup \italic "loco"
}
