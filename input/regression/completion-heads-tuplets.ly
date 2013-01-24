\version "2.17.11"

\header {
  texidoc = "
Completion heads may be used with tuplets (and compressed music) too.
"
}

\layout {
  \context {
    \Voice
    \remove "Note_heads_engraver"
    \consists "Completion_heads_engraver"
  }
}

\context Staff \relative c'' {
  \tupletSpan 1
  \tuplet 3/2 { g1 g g }
}
