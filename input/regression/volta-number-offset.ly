\version "2.25.6"

\header {
  texidoc = "The offset of the volta number can be controlled with the
property @code{volta-number-offset}."
}

{
  \repeat volta 2 {
    f'1
    \alternative {
      \volta 1 {
        \once \override Score.VoltaBracket.volta-number-offset = #'(0 . 0)
        e'1 }
      \volta 2 {
        d'1 }
    }
  }
}
