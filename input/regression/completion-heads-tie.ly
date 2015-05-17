\header {

  texidoc = "Completion heads will remember ties, so they are started
  on the last note of the split note."

  }

\version "2.19.21"

\paper{ ragged-right=##t }

\new Staff \new Voice \with { 
  \remove "Note_heads_engraver"
  \consists "Completion_heads_engraver"
} {
  \relative { r2 d''1 ~ 1 d2 }
}

