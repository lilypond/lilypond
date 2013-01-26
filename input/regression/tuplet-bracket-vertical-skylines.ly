\version "2.17.11"

\header {
  texidoc = "Tuplet brackets do not push objects with outside-staff-priority
too high.
"
}

\relative c' {
  \override TupletBracket.direction = #UP
  \tuplet 1/1 { a^"foo" a' a' a' }
}
