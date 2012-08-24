\version "2.16.0"

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
  \set tupletSpannerDuration = #(ly:make-moment 1 1)
  \times 2/3 { g1 g g }
}
