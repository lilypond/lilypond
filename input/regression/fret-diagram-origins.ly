\version "2.17.6"

\header {
  texidoc="
Fret diagrams of different orientation should share a common origin
of the topmost fret or string.

"
}

\include "predefined-guitar-fretboards.ly"
\layout { ragged-right = ##t }
<<
  \chords {
    c1 |
    c1 |
    c1
  }

  \new FretBoards {
    \chordmode{
    c1 |
    \override FretBoard.fret-diagram-details.orientation = #'landscape
    c1 |
    \override FretBoard.fret-diagram-details.orientation = #'opposing-landscape
    c1
    }
  }

  \new Voice {
    c'1 |
    c'1 |
    c'
 }
>>


