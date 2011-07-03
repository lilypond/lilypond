\version "2.14"

\header {
  texidoc = "
A @code{\Voice} should be able to contain both an @code{Ambitus_engraver}
and a @code{Mensural_ligature_engraver} without segfaulting.
  "
}

\score{
  {
    \context Staff="default" {
      \[ c'\longa c''\longa \]
    }
  }
  \layout {
    \context{
      \Voice
      \consists Ambitus_engraver
      \consists Mensural_ligature_engraver
    }
  }
}
