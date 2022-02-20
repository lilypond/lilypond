\version "2.19.21"

\header {

  texidoc = "A volta repeat may begin with a grace. Consecutive
    ending and starting repeat bars are merged into one @code{:..:}."

}

\layout { ragged-right= ##t }

\relative {
  \repeat volta 2 {
    c'1
  }
  \repeat volta 2 {
    \grace {c8 } c4
  }
}
