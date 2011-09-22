\version "2.15.13"

\header {
  texidoc = "Loose columns (here, the treble clef) are spaced
correctly in polyphonic music.
"
}

\new PianoStaff <<
  \new Staff \relative c' {
    \times 2/3 { g'4 a2 }
  }
  \new Staff \relative c' {
    \clef bass fis,,8 cis'
    \clef treble g'' fis,
  }
>>
