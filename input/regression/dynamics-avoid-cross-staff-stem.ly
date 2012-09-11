\version "2.16.0"

\header {
  texidoc = "LilyPond automatically shifts dynamics that collide with
cross-staff stems when manual beams are used."
}

\new GrandStaff <<
  \new Staff = "PnRH" {
    \relative g {
      \stemDown gis8 \p [ \change Staff = "PnLH" \stemUp a, \fff ]
      \change Staff = "PnRH" r4
    }
  }
  \new Staff = "PnLH" { \clef "F" { s4 r4 } }
>>
