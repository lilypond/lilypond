\header {

  texidoc = "With the @code{padding-pairs} property, distances
 between individual key signature items can be adjusted."

}

\version "2.11.51"

{
  \override Staff.KeySignature #'padding-pairs
    = #'((("accidentals.flat" . "accidentals.sharp.slashslash.stemstemstem") . 0.5))
    \set Staff.keySignature = #`((2 . ,SEMI-FLAT)  (6 . ,THREE-Q-SHARP) (4 . ,FLAT))
  e2
}
