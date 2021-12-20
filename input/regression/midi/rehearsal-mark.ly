\version "2.23.6"

\header {
  texidoc="Rehearsal marks create MIDI Marker events."
}

\score {
  \fixed c' {
    c1 \mark \default d1 \mark \default e1 \mark 23 f1
  }
  \midi {}
}
