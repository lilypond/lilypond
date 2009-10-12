\version "2.13.4"

#(set-default-paper-size "a6")

\book {
  \header {
    texidoc = "Padding before titles is honoured by the page breaker.
This should take up two pages."
  }

  \paper {
    before-title-spacing = #'((padding . 300))
  }

  \markup "00"
  \markup "01"
  \score {
    { c'1 \allowPageTurn }
  }
  \markup "02"
}
