\header {

  texidoc = "Completion heads will remember ties, so they are started
  on the last note of the split note."

  }

\version "2.11.58"

\paper{ ragged-right=##t }

\new Staff \new Voice \with { 
  \remove "Note_heads_engraver"
  \consists "Completion_heads_engraver"
} {
  \relative c'' { r2 d1 ~ d1 d2 }
}

