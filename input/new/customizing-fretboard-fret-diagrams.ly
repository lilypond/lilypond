\version "2.11.55"

\header {
  lsrtags = "fretted-strings,tweaks-and-overrides"

  texidoc = "Fret diagram properties can be set through
@code{fret-diagram-details}.  For FretBoard fret diagrams,
overrides are applied to the @code{FretBoards.FretBoard} object.
"
  doctitle = "Customizing fretboard fret diagrams"
}

\include "predefined-guitar-fretboards.ly"
\storePredefinedDiagram \chordmode { c' }
                        #guitar-tuning
                        #"x;1-1-(;3-2;3-3;3-4;1-1-);"
<<
  \context ChordNames {
    \chordmode { c1 c c d }
  }
  \context FretBoards {
    % Set global properties of fret diagram
    \override FretBoards.FretBoard #'size = #'1.2
    \override FretBoards.FretBoard #'fret-diagram-details
                                  #'finger-code = #'in-dot
    \override FretBoards.FretBoard #'fret-diagram-details
                                  #'dot-color = #'white
    \chordmode {
      c
      \once \override FretBoards.FretBoard #'size = #'1.0
      \once \override FretBoards.FretBoard #'fret-diagram-details
              #'barre-type = #'straight
      \once \override FretBoards.FretBoard #'fret-diagram-details
              #'dot-color = #'black
      \once \override FretBoards.FretBoard #'fret-diagram-details
              #'finger-code = #'below-string
      c'
      \once \override FretBoards.FretBoard #'fret-diagram-details
              #'barre-type = #'none
      \once \override FretBoards.FretBoard #'fret-diagram-details
              #'number-type = #'arabic
      \once \override FretBoards.FretBoard #'fret-diagram-details
              #'orientation = #'landscape
      \once \override FretBoards.FretBoard #'fret-diagram-details
              #'mute-string = #"M"
      \once \override FretBoards.FretBoard #'fret-diagram-details
              #'label-dir = #-1
      \once \override FretBoards.FretBoard #'fret-diagram-details
              #'dot-color = #'black
      c'
      \once \override FretBoards.FretBoard #'fret-diagram-details
              #'finger-code = #'below-string
      \once \override FretBoards.FretBoard #'fret-diagram-details
              #'dot-radius = #0.35
      \once \override FretBoards.FretBoard #'fret-diagram-details
              #'dot-position = #0.5
      \once \override FretBoards.FretBoard #'fret-diagram-details
              #'fret-count = #3
      d
    }
  }
  \context Voice {
    c'1 c' c' d'
  }
>>
