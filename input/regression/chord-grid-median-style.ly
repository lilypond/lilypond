\version "2.23.10"

\header {
  texidoc = "In chord grids, the @code{\\medianChordGridStyle} command causes
measures split in 4@tie{}equal parts to be printed with median rather than
diagonal lines.  This is the style recommended in Philippe Baudoin's book
@emph{Jazz, mode d'emploi}."
}

\paper {
  indent = 0
}

\new ChordGrid \chordmode {
  \medianChordGridStyle
  c1
  c2:6 c2
  g4 g4 c2
  c2 g4 g4
  \break
  c4 c4:m7 c4:aug7 c4/g
  c2. g4
  c4 g2.
  c1
}
