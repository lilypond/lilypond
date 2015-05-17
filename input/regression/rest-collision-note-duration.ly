\version "2.19.21"

\header { texidoc =
"Vertical rest positions in a multi-voice staff should obey the duration of
notes; this is, they shouldn't return to a default position too early.
"
}

\relative {
  << { g'1  g2 } \\
     { \repeat unfold 2 {r8 d4 d8 r d4 d8} } >>
}
