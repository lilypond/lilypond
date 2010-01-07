\version "2.13.3"
\header {
  texidoc = "By setting the padding between systems to a negative
value, it is possible to eliminate the anti-collision constraints.
"
}

\book {
  \paper {
      ragged-bottom =##t
      between-system-spacing = #'((space . 1) (padding . -10))
  }

  {
    c1 \break
    c'''1
  }
}
