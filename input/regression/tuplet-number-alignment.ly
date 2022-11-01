\version "2.23.82"

\header {
  texidoc = "Tuplet numbers of flat beams vertically align with
similar looking beams."
}

\relative c''' {
  \tuplet 3/2 { b8 c d }
  \tuplet 3/2 { e8 f g }
}

\relative c''' {
  \tuplet 3/2 { b16 c d }
  \tuplet 3/2 { e16 f g }
}
