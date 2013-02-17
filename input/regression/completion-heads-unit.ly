\version "2.17.11"

\header {
texidoc = "
Note head completion may be broken into sub-bar units by setting the
@code{completionUnit} property.
"
}

\layout {
  \context {
    \Voice
    \remove "Note_heads_engraver"
    \consists "Completion_heads_engraver"
  }
}

\context Staff \relative f {
  \time 9/8
  \set completionUnit = #(ly:make-moment 3/8)
  g'1.. g2
  \time 6/4
  \set completionUnit = #(ly:make-moment 1/4)
  \tupletSpan 4
  \tuplet 3/2 { e4 c8 f g a4 b8 c4 b8 a4 g8 a e f4 }
}
