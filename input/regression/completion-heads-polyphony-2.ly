\version "2.16.0"

\header {
  texidoc = "
Complex completion heads work properly in a polyphonic environment.
"
}

\layout {
  \context {
    \Voice
    \remove "Note_heads_engraver"
    \consists "Completion_heads_engraver"
  }
}

\context Staff \relative c'' <<
  { c4. c c c4 c4. c4 }
  \\
  { g8 g2 g1 g4. }
>>
