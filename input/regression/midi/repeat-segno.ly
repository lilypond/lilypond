\version "2.23.6"

\header {
  texidoc="Segni and coda signs create MIDI Marker events."
}

\score {
  \fixed c' {
    \repeat unfold 3 {
      c1
      \repeat segno 2 {
        d1
        \alternative {
          e1
          <>
        }
      }
      f1
    }
  }
  \midi {}
}
