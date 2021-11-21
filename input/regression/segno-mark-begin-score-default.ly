\version "2.23.6"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc = "@code{\\segnoMark \\default} at the beginning of the
score does not create a mark.  A single segno should appear at the
beginning of the second measure and a double segno should appear at
the end."
}

piece = \new Staff {
  \segnoMark \default R1 |
  \segnoMark \default R1 |
  %% Known issue: When segnoStyle is 'bar-line, the segno bar line is
  %% not printed at the end of the piece.
  \segnoMark \default
}

\new Score \with { segnoStyle = #'bar-line } \piece
\new Score \with { segnoStyle = #'mark } \piece
