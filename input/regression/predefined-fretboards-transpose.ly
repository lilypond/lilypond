\version "2.16.0"
\header{

  texidoc="
Transposition by less than one octave up or down should not affect
predefined fretboards.

"
}


\include "predefined-guitar-fretboards.ly"

myChords = \chordmode { c1 d }

mySequence = {

  \myChords
  \transpose c c' {\myChords}
  \transpose c c, { \myChords}
}

<<
  \new ChordNames {\mySequence}
  \new FretBoards {\mySequence}
>>

