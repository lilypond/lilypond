\version "2.19.21"

\header {
  texidoc = "Placement of beamed cross staff rests should be
reasonably close to beam.
"
}

Up = \change Staff = "up"
Down = \change Staff = "down"

\new PianoStaff <<
  \new Staff = "up"
  \relative {
    \time 6/8 a'2.
    c'8[ \Down c,16 \Up fis \Down r fis] \Up
  }
  \new Staff = "down" { s2. s4. }
>>
