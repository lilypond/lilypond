\version "1.3.146"

scheme = \notes {
  <c'1 e' g'>
  <e' g' c''>
  <e e' g' c''>
}

\score {
  <
    \context ChordNamesVoice \scheme
    \context Staff \scheme
  >
  \paper { linewidth = -1. }
}
