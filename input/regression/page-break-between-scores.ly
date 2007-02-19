\version "2.11.19"

\header {
  texidoc = "Page breaks work when they are placed at the end of a score."
}

#(set-default-paper-size "a6")
\book {
  \score {
    {a b c' d' \pageBreak}
  }
  \score {
    {a b c' d'}
  }
}
