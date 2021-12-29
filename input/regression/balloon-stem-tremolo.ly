\version "2.23.6"

\header {
  texidoc = "Balloons work on stem tremoli."
}

\new Staff \with {
  \consists Balloon_engraver
} {
  a'8[
  \balloonGrobText StemTremolo #'(0 . 3) StemTremolo
  a'8:32]
}
