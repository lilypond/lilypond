\version "2.23.11"

\header {
  texidoc = "In chord grids, lines inside squares attach to the
innermost line of the bar line."
}

\paper {
  ragged-right = ##f
}

\new ChordGrid \chordmode {
  \bar "[|:"
  c4 4 4 4 4 4 4 4
  \bar ":|][|:"
  c4 4 4 4 4 4 4 4
  \bar ":|]"
}
