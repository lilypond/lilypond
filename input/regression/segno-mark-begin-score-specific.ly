\version "2.23.6"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc = "@code{\\segnoMark 1} at the beginning of the score
creates a visible mark.  A single segno should appear at the beginning
of the measure and a double segno should appear at the end."
}

piece = \new Staff {
  \segnoMark 1 R1 |
  %% Known issue: When segnoStyle is 'bar-line, the segno bar line is
  %% not printed at the end of the piece.
  \segnoMark \default
}

\new Score \with { segnoStyle = #'bar-line } \piece
\new Score \with { segnoStyle = #'mark } \piece
