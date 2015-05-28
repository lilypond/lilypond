\version "2.19.21"

\header {
  texidoc = "Tuplet brackets do not push objects with outside-staff-priority
too high.
"
}

\relative {
  \override TupletBracket.direction = #UP
  \tuplet 1/1 { a^"foo" a' a' a' }
}
