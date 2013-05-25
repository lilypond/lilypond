\version "2.16.0"

\header {

  texidoc="Transposition symbols should be correctly positioned
close to the parent clef."

}
\score {
  <<
    \new Staff { \clef "G^8" g''1 }
    \new Staff { \clef "F^8" c'1 }
    \new Staff { \clef "C^8" c''1 }
    \new Staff { \clef "G_8" g1 }
    \new Staff { \clef "F_8" c,1 }
    \new Staff { \clef "C_8" c1 }
  >>
}
