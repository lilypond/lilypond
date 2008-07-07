\version "2.11.51"

\header {
  texidoc = "Accidentals can be forced with ! and ? even if the notes are tied."
}

\layout {
  ragged-right = ##t
}

\relative {
  gis'4 ~ gis!~ gis?
  }
