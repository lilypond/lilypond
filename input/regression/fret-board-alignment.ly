\header {

  texidoc = "FretBoards should be aligned in the Y direction
at the fret-zero, string 1 intersection."

}

\version "2.16.0"

\include "predefined-guitar-fretboards.ly"

mychords = \chordmode{
  c1 f g
}

<<
  \context ChordNames {
    \mychords
  }
  \context FretBoards {
    \mychords
  }
>>
