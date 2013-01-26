\version "2.16.0"

\header {
  texidoc = "
A @code{\\Voice} should be able to contain both an @code{Ambitus_engraver}
and a @code{Mensural_ligature_engraver} without segfaulting.
  "
}

\new Voice \with  {
  \consists "Ambitus_engraver"
  \consists "Mensural_ligature_engraver"
  \remove "Ligature_bracket_engraver"
} {
  \cadenzaOn
  \[ c'\longa c''\longa \]
}
