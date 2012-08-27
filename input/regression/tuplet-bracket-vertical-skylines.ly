\version "2.15.28"

\header {
  texidoc = "Tuplet brackets do not push objects with outside-staff-priority
too high.
"
}

\relative c' {
  \override TupletBracket #'direction = #UP
  \times 1/1 { a^"foo" a' a' a' }
}
