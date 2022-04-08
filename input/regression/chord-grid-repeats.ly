\version "2.23.10"

\header {
  texidoc = "Repeat notation can be used in chord grids."
}

\paper {
  indent = 0
  ragged-right = ##f
  system-system-spacing.padding = 4
}

\new ChordGrid \chordmode {
  \repeat percent 2 { c1 }
  c1 c1
  \break
  %% FIXME: uneven spacing around the DoublePercentRepeat.
  \repeat percent 2 { c1 c1 }
  \break
  %% Inner lines should stop at the "|" part of ".|:", not
  %% at ":".
  \repeat volta 2 {
    c2:m c4:m c4:m
    e1
    g2 g2
    g4 g4 g4 g4
  }
  \break
  \repeat segno 2 {
    c2:m c4:m c4:m
    e1
    e1
    e1
  }
}
