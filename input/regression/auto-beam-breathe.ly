\version "2.19.21"

\header {
  texidoc = "Automatic beams are ended early if a breathing sign is
encountered."
}

\relative {
  \time 1/1
  \repeat unfold 8 c'8
  c8 c
  \breathe
  c8 c c c c
  \breathe
  c8
}
