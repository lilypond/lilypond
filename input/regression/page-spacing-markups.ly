\version "2.15.17"

\header {
  texidoc = "Adjacent lines of markup are placed as closely
together as possible."
}

#(set-default-paper-size "a6")
\book {
  \paper {
    ragged-last-bottom = ##f
  }
  \markuplist {
    \column { A B } C D E
  }
}
