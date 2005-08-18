\version "2.6.0"

\header {

  texidoc = "A volta repeat may begin with a grace. Consecutive
    ending and starting repeat bars are merged into one @code{:||:}."

}

\layout { raggedright= ##t }

\relative c' {
  \repeat volta 2 {
    c1 
  }
  \repeat volta 2 {
    \grace {c8 } c4
  }
}
