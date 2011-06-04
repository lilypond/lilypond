\version "2.14.0"

\header {
  texidoc = "Accidentals can be forced with ! and ? even if the notes are tied."
}

\layout {
  ragged-right = ##t
}

\relative c'' {
  gis4 ~ gis!~ gis?
}
