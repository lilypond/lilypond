
\version "2.19.25"

\header {
  texidoc="Markups can be put into the dots of a fret-diagram.  Those markups
are scaled automatically to fit into the dots.
"
}

myFretDiagram =
\markup
  \fret-diagram-verbose #`(
    (place-fret 6 1
       ,(markup
         #:concat (
           #:vcenter "e"
           #:fontsize -5
           #:musicglyph "accidentals.sharp")))
    (place-fret 5 2 "aisis")
    (place-fret 4 3 3)
    (place-fret 3 1 "g#")
    (place-fret 2 2 2)
    (place-fret 1 3 ,#{ \markup \score { { \clef "G_8" g'1 } } #}))


\markup
  \override #'(fret-diagram-details . ((finger-code . in-dot))) {
  \myFretDiagram
  \override #'(size . 5) \myFretDiagram
  \override #'(size . 10) \myFretDiagram
}
