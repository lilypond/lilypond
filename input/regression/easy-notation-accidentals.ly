\version "2.16.0"

\header {
  texidoc = "
Accidentals are positioned correctly when using Easy notation.
"
}

\relative c' {
  \easyHeadsOn
  c4 cis cisis2
  e4 ees eeses2
}
