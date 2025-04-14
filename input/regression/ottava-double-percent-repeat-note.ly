\version "2.25.26"

\header {
  texidoc = "Ottava brackets include double-percent repeat measures.  In this
test, the percent repeat is followed by a note."
}

#(ly:set-option 'warning-as-error #t)

\fixed c'' {
  \ottava 1
  \repeat percent 2 { c4 d e f | g f e d }
  c1
  \ottava 0
}

\fixed c'' {
  \ottava 1
  \repeat percent 2 { c4 d e f | g f e d }
  \ottava 0
  c,1^\markup \italic "loco"
}
