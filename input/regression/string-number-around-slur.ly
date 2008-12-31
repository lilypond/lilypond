\version "2.12.0"

\header {
  texidoc = "String numbers should only be moved outside slurs when there
is a collision."
}

\relative c'' {
  \textLengthOn
  <c\3>4-"inside"( d' <e,\2>-"inside" g
  <c\1>1-"outside")
}
