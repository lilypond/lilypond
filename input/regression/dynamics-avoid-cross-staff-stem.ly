\version "2.17.10"

\header {
  texidoc = "Dynamics are correctly nested over/under cross staff stems.
They are, however, not yet factored into horizontal spacing - the fff
collides with other grobs.
"
}

\new GrandStaff <<
  \new Staff = "PnRH" {
    \relative g {
      \stemDown gis8 \p [ \change Staff = "PnLH" \stemUp a, \fff ]
      a8 \p [ \change Staff = "PnRH" \stemDown gis'8 \fff ]
      \change Staff = "PnRH" r4
    }
  }
  \new Staff = "PnLH" { \clef "F" { s2 r4 } }
>>
