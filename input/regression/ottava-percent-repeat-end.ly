\version "2.25.35"

\header {
  texidoc = "Ottava brackets include single-percent repeat measures.  In this
test, the ottavation is not canceled."
}

#(ly:set-option 'warning-as-error #t)

\fixed c'' {
  \ottava 1
  \%2 { s1 }
}

\fixed c'' {
  \ottava 1
  \%2 { R1 }
}
