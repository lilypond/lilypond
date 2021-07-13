\version "2.23.4"

\header {
  texidoc = "Lyrics may receive footnotes."
}

\book {
  \new Lyrics \lyricmode {
    \footnote #'(2 . -2) "LyricText" LyricText
    aaah
  }
}
