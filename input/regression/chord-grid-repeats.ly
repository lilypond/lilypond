\version "2.25.0"

\header {
  texidoc = "Repeat notation can be used in chord grids.

For volta repeats, a repeat bar line is printed even at the
beginning of the piece.  Inner lines in the chord square should
stop at the @samp{|} part of the @samp{.|:} bar line, not at
@samp{:}."
}

\paper {
  indent = 0
  ragged-right = ##f
  system-system-spacing.padding = 4
}

\new ChordGrid \chordmode {
  \repeat volta 2 {
    c2:m c4:m c4:m
    e1
    g2 g2
    g4 g4 g4 g4
  }
  \break
  \repeat percent 2 { c1 }
  c1 c1
  \break
  %% FIXME: uneven spacing around the DoublePercentRepeat.
  \repeat percent 2 { c1 c1 }
  \break
  \repeat segno 2 {
    c2:m c4:m c4:m
    e1
    e1
    e1
  }
}
