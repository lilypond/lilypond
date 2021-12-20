\version "2.23.6"

\header {
  texidoc="Section labels create MIDI Marker events."
}

\score {
  \fixed c' {
    \sectionLabel \markup \smallCaps "Intro" c1
    \sectionLabel "Coda" d1
  }
  \midi {}
}
