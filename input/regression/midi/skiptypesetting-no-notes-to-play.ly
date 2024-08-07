\version "2.25.19"

\header {

texidoc = "skipTypesetting no notes to play, create emtpy MIDI file"

}


\score {
  {
    \set Score.skipTypesetting = ##t
    c1
  }
  \midi { }
}
