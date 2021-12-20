\version "2.23.6"

\header {
  texidoc="Variant coda signs create MIDI Marker events."
}

\score {
  \fixed c' {
    \set Score.codaMarkFormatter = #format-varcoda-mark
    c1
    \codaMark \default
    d1
  }
  \midi {}
}
