\header {
  texidoc = "Skylines cover all segments of slurs."
}

\version "2.20.0"

#(ly:set-option 'debug-skylines #t)

\relative g'' {
  g4 (d' b g) |
  g4 \(d' b g\)
}
