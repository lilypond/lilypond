\version "2.27.0"

\header {
  texidoc = "Using microtones in chord names works."
}

Chords = \chordmode {
  cih1
  cih:7
  cih:9+
  cih:11-

  cih/gisih
  cih/aih
  cih/+fih
  cih/+gih \bar "||" \break

  ceh1
  ceh:7
  ceh:9+
  ceh:11-

  ceh/geseh
  ceh/aeh
  ceh/+feh
  ceh/+geh \bar "||" \break
}

<<
  \new ChordNames \Chords
  \new Voice \Chords
>>
