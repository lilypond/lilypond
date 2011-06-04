\version "2.14.0"

\header {
  texidoc = "There is a variant of the arpeggio sign that uses a
`vertical slur' instead of the wiggle."
}

\relative c' {
  \arpeggioParenthesis
  % Note: does not work for cross staff arpeggios.
  <c g' c>2\arpeggio ~ c
}
