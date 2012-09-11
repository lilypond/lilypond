\version "2.16.0"

\header {
  texidoc = "Cross staff beams do collision avoidance.
"
}

\new PianoStaff <<
  \new Staff = up \relative c' { s2 }
  \new Staff \relative c' {
    \clef bass \time 2/4
    g,32 [ d' \change Staff = up d'
    gis a c d g ] r4 |
  }
>>
