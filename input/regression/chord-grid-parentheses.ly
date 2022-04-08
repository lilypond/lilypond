\version "2.23.10"

\header {
  texidoc = "Individual chords can be parenthesized in chord grids."
}

\paper {
  ragged-right = ##f
}

\new ChordGrid \chordmode {
  \override Parentheses.font-size = 0
  c1 \parenthesize c1 d1 \parenthesize d1
}
