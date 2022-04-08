\version "2.23.10"

\header {
  texidoc = "Chord grids may contain skips.  They cause a blank
space in chord squares."
}

\paper {
  indent = 0
  ragged-right = ##f
}

\new ChordGrid \chordmode {
  c2 s2
  % Consecutive skips are merged.
  c2 s4 s4
  % Individual skips need not be aligned with the boundaries
  % of cells in the square.  Only the merged skips resulting
  % from consecutive skips need to be.
  c2 s4. s8
  % Skips can take a whole measure.
  s1
}

\new ChordGrid \chordmode {
  % Skips can start the chord grid.  They can also
  % straddle over measure boundaries.
  s1*3/2 c2
  % Skips can take several measures.
  s1*2
}
