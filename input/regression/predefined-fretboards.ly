\version "2.21.0"

\header {
texidoc = "Predefined fretboards and chord shapes can be added.
"
}

% add a chord shape

\addChordShape #'bes #guitar-tuning "x;1-1-(;3-2;3-3;3-4;1-1-);"

% add chords

\storePredefinedDiagram #default-fret-table \chordmode {bes}
                        #guitar-tuning
                        #(chord-shape 'bes guitar-tuning)

\storePredefinedDiagram #default-fret-table \chordmode {c}
                        #guitar-tuning
                        #(offset-fret 2 (chord-shape 'bes guitar-tuning))

mychords = \chordmode {
  bes
}

chordline = {
  \mychords
  \transpose bes c {
    \mychords
  }
}

<<
  \context ChordNames {
    \chordline
  }
  \context FretBoards {
    \chordline
  }
>>
