\header {

    texidoc= "Chord names don't attempt to find inversions and
bass notes."  }

\version "1.5.68"

scheme = \notes {
  <c'1 e' g'>
  <e' g' c''>
  <e e' g' c''>
}

\score {
  <
    \context ChordNames \scheme
    \context Staff \scheme
  >
  \paper { linewidth = -1. }
}
