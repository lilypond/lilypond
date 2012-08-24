\version "2.16.0"

\header {
  texidoc = "By setting the padding between systems to a negative
value, it is possible to eliminate the anti-collision constraints.
"
}

\book {
  \paper {
    ragged-bottom = ##t
    system-system-spacing = #'((basic-distance . 1) (padding . -10))
  }

  {
    c1 \break
    c'''1
  }
}
