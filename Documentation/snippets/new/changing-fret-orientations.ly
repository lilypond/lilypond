\version "2.13.27"

\header {
  lsrtags = "fretted-strings"

  texidoc = "Fret diagrams can be oriented in three ways.  By default
+the top string or fret in the different orientations will be aligned.
"
  doctitle = "Changing fret orientations"
}

\include "predefined-guitar-fretboards.ly"
\layout { ragged-right = ##t }
<<
  \chords {
    c1
    c1
    c1
  }
  \new FretBoards {
    \chordmode {
    c1
    \override FretBoard #'fret-diagram-details
       #'orientation = #'landscape
    c1
    \override FretBoard #'fret-diagram-details
       #'orientation = #'opposing-landscape
    c1
    }
  }
  \new Voice {
    c'1
    c'1
    c'
  }
>>
