\version "2.25.26"

\header {
  texidoc = "Ottava brackets extend past trill lines."
}

#(ly:set-option 'warning-as-error #t)

\fixed c'' {
  \ottava 1
  c'1\trill
}

\fixed c'' {
  \ottava 1
  c'1\startTrillSpan
  <>\stopTrillSpan
}

\fixed c'' {
  \ottava 1
  c'1\startTrillSpan
  <>\stopTrillSpan
  \ottava 0 \bar "||"
}

\fixed c'' {
  \ottava 1
  c'2\startTrillSpan
  \ottava 0 \bar "||"
  c'2\stopTrillSpan
}

\fixed c'' {
  \ottava 1
  c'2\startTrillSpan
  c'2\stopTrillSpan
  \ottava 0 \bar "||"
}
