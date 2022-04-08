\version "2.23.10"

\header {
  texidoc = "Within chord grids, an unterminated measure should be handled gracefully."
}

#(ly:set-option 'warning-as-error)
#(ly:expect-warning (G_ "unterminated measure for chord square"))

\paper {
  indent = 0
}

\new ChordGrid \chordmode { c1 c2 }
