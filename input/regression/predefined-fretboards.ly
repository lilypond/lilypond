% add a chord shape

\addChordShape #'bes #"x;1-1-(;3-2;3-3;3-4;1-1-);"

% add chords

\storePredefinedDiagram \chordmode {bes}
                        #guitar-tuning
                        #(chord-shape 'bes)

\storePredefinedDiagram \chordmode {c}
                        #guitar-tuning
                        #(offset-fret 2 (chord-shape 'bes))

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
