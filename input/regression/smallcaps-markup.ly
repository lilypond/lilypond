\version "2.23.12"

\header {
  texidoc = "@code{\\smallCaps} works on an arbitrary markup argument."
}

\paper {
  scoreTitleMarkup = \markup \smallCaps \fromproperty #'header:piece
}

\score {
  \header {
    piece = "Gavotte"
  }
  { c' }
}
