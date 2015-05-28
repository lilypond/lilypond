\version "2.19.21"

\header {
  texidoc = "Loose columns (here, the treble clef) are spaced
correctly in polyphonic music.
"
}

\new PianoStaff <<
  \new Staff \relative {
    \tuplet 3/2 { g'4 a2 }
  }
  \new Staff \relative {
    \clef bass fis,8 cis'
    \clef treble g'' fis,
  }
>>
