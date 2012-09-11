\version "2.16.0"

#(set-default-paper-size "a6")

\book {
  \header {
    texidoc = "Padding between a score and a markup is honored by
the page breaker.  This should take up two pages."
  }

  \paper {
    score-markup-spacing = #'((padding . 300))
  }

  \markup "00"
  \markup "01"
  \score {
    { c'1 \allowPageTurn }
  }
  \markup "02"
}
