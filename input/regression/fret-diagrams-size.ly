\version "2.16.0"

\header {
  texidoc="
Fret diagrams can be scaled using the @code{size} property.
Also, scaling the distance between the frets and/@/or strings is possible with
the properties @code{fret-distance} and/@/or @code{string-distance} of
@code{fret-diagram-details}.
The position and size of first fret label, mute/@/open signs, fingers,
relative to the diagram grid, shall be the same in all cases.

"
}

myFretDiagram =
\markup
  \override #'(fret-diagram-details . ((finger-code . below-string)))
  \fret-diagram-verbose #'((place-fret 6 6 "P")
                           (place-fret 5 8 2)
                           (place-fret 4 8 2)
                           (place-fret 3 7 1)
                           (place-fret 2 8 3)
                           (mute 1))

\markup
  {
    \myFretDiagram
    \override #'(size . 1.5) \myFretDiagram
    \override #'(size . 3) \myFretDiagram
  }

\markup
  {
    \override #'(fret-diagram-details . ((fret-distance . 0.5)))
    \myFretDiagram
    \override #'(fret-diagram-details . ((fret-distance . 1.5)))
    \myFretDiagram
    \override #'(fret-diagram-details . ((fret-distance . 2.5)))
    \myFretDiagram
  }

\markup
  {
    \override #'(fret-diagram-details . ((string-distance . 0.5)))
    \myFretDiagram
    \override #'(fret-diagram-details . ((string-distance . 1.5)))
    \myFretDiagram
    \override #'(fret-diagram-details . ((string-distance . 2.5)))
    \myFretDiagram
  }
