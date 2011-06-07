\version "2.14.0"

\header {
  texidoc = "Automatic beams are ended early if a breathing sign is
encountered."
}

\relative c' {
  \time 1/1
  \repeat unfold 8 c8
  c8 c
  \breathe
  c8 c c c c
  \breathe
  c8
}
