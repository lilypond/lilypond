\version "2.16.0"

\header {
  texidoc = "Placement of beamed cross staff rests should be
reasonably close to beam.
"
}

Up = \change Staff = "up"
Down = \change Staff = "down"

\new PianoStaff <<
  \new Staff = "up"
  \relative c'' {
    \time 6/8 a2.
    c'8[ \Down c,16 \Up fis \Down r fis] \Up
  }
  \new Staff = "down" { s2. s4. }
>>
