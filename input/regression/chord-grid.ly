\version "2.23.10"

\header {
  texidoc = "The @code{ChordGrid} context creates chord grid notation."
}

\paper {
  indent = 0
  system-system-spacing.padding = 2.5
}

\new ChordGrid \chordmode {
  c1
  c2:6 c2
  g4 g4 c2
  c2 g4 g4
  \break
  c4 c4:m7 c4:aug7 c4/g
  c2. g4
  c4 g2.
  c1
}
