\version "2.19.21"

\header {
  texidoc = "Accidentals can be forced with ! and ? even if the
notes are tied.  Cautionary accidentals applied to tied notes
after a bar line are valid for the whole measure.
"
}

\layout {
  ragged-right = ##t
}

\relative {
  gis'4 ~ gis!~ gis? r4
  fis1 ~
  fis!2 fis ~
  fis?2 fis
}

