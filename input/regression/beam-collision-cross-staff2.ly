\version "2.19.21"

\header {
  texidoc = "Cross staff beams do collision avoidance.
"
}

\new PianoStaff <<
  \new Staff = up \relative c' { s2 }
  \new Staff \relative {
    \clef bass \time 2/4
    g,32 [ d' \change Staff = up d'
    gis a c d g ] r4 |
  }
>>
