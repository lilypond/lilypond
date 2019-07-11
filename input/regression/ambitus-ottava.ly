\version "2.21.0"

\header {
  texidoc = "A voice starting with \ottava shouldn’t confuse ambitus."
}

\layout {
  \context {
    \Staff
    \consists Ambitus_engraver
  }
}

{
  \clef alto
  \ottava 1
  d' b'
  \ottava 0
  g'
}
