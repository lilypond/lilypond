\version "2.27.0"

\header {
  texidoc = "Italian chord names have more space before and after the
accidentals for roots."
}

\chords {
  \italianChords
  d:9 dis:9 des:9
}

\chords {
  \override ChordName.font-size = #0
  \italianChords
  d:9 dis:9 des:9 \break
}
