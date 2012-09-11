\version "2.16.0"
\header
{
  texidoc =
  "Tablatures derived from stored fretboard diagrams display open strings
as fret 0 in the tablature.  The tablature and fretboard should match."
}

#(define c-shape-fretboard-table (make-fretboard-table))

\storePredefinedDiagram #c-shape-fretboard-table
                         \chordmode{c}
                         #guitar-tuning
                         "x;3-3;2-2;o;1-1;o;"

cShape = {
   \set predefinedDiagramTable = #c-shape-fretboard-table
}

Chords = \chordmode {
   \cShape
   c,1 |
}

\score {
     <<
       \new ChordNames {
            \Chords
       }
       \new FretBoards {
         \Chords
       }
       \new Staff {
         \clef "treble_8"
         \Chords
       }
       \new TabStaff  {
         \Chords
       }
     >>
}
