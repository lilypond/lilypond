\version "2.13.8"

\header {
  texidoc = "
A repetition symbol can be used to repeat the previous chord
and save typing.  Only note events are copied.
"
}

\relative c' {
  <c e g>8\p( q) q4-| q8.\(^"text" q16 q4-|\)
}
