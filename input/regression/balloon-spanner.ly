\header {
  texidoc = "Balloons on spanners, such as slurs, are supported."
}

\version "2.21.2"

\relative c' \new Voice \with { \consists Balloon_engraver } {
  \balloonGrobText Slur #'(0 . -1) "X"
  c (  d)
}
