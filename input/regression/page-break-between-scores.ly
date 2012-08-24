\version "2.16.0"

\header {
  texidoc = "Page breaks work when they are placed at the end of a score,
or between scores."
}

#(set-default-paper-size "a6")
\book {
  \score {
    {a b c' d' \pageBreak}
  }
  \score {
    {a b c' d'}
  }
  \pageBreak
  \score {
    {a b c' d'}
  }
}
