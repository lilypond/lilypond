\version "2.19.21"

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

\context Staff \relative <<
  { c''4. c c c4 c4. c4 }
  \\
  { g8 g2 g1 g4. }
>>
