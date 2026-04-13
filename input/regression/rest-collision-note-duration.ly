\version "2.25.35"

\header { texidoc =
"Vertical rest positions in a multi-voice staff should obey the duration of
notes; this is, they shouldn't return to a default position too early.
"
}

\relative {
  << { g'1  g2 } \\
     { \*2 { r8 d4 d8 r d4 d8} } >>
}
