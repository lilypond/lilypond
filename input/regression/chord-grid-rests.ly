\version "2.23.10"

\header {
  texidoc = "Chord grids can contain rests.  This causes the @code{noChordSymbol}
to be printed."
}

\paper {
  indent = 0
  ragged-right = ##f
}

\new ChordGrid \chordmode {
  c1
  r1
  c2 r2
  c2. r4
  R1
}
