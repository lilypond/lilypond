\version "2.23.4"

\header {
  texidoc = "Automatic beams may receive footnotes."
}

\book {
  {
    \footnote #'(2 . 2) "Beam" Beam
    c8 8 8 8
  }
}
