\version "2.23.10"

\header {
  texidoc = "Chord grids are properly scaled with staff size."
}

%% FIXME: not all thicknesses are properly scaled yet.

\paper {
  indent = 0
  ragged-right = ##f
  score-system-spacing.padding = 5
}

\new ChordGrid \with {
  \magnifyStaff #1/2
}
\chordmode { c1 c1 c1 c1 c1 c1 c1 c1 }

\new ChordGrid \chordmode { c1 c1 c1 c1 }

\new ChordGrid \with {
  \magnifyStaff #2
}
\chordmode { c1 c1 }
