\version "2.25.26"

\header {
  texidoc = "Ottava brackets include double-percent repeat measures.  In this
test, the percent repeat is followed by a whole-measure rest.  Double bar lines
show where the ottavation is canceled."
}

#(ly:set-option 'warning-as-error #t)

\fixed c'' {
  \ottava 1
  \repeat percent 2 { c4 d e f | g f e d }
  \ottava 0 \bar "||"
  R1
}

\fixed c'' {
  \ottava 1
  \repeat percent 2 { c4 d e f | g f e d }
  R1
  \ottava 0 \bar "||"
}
