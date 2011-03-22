\version "2.13.51"

\header {
  texidoc="
Fret diagrams can be scaled using the @code{size} property.
The position and size of first fret label, mute/open signs, fingers,
relative to the diagram grid, shall be the same in all cases.

"
}

myFretDiagram =
\markup \fret-diagram-verbose #'((place-fret 6 6 "P")
                                 (place-fret 5 8 2)
                                 (place-fret 4 8 2)
                                 (place-fret 3 7 1)
                                 (place-fret 2 8 3)
                                 (mute 1))

\markup \override #'(fret-diagram-details . ((finger-code . below-string))) {
  \myFretDiagram
  \hspace #4
  \override #'(size . 1.5) \myFretDiagram
  \hspace #8
  \override #'(size . 3) \myFretDiagram
}
