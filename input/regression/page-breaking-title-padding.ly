\version "2.13.4"

#(set-default-paper-size "a6")

\book {
  \header {
    texidoc = "Padding between titles is honoured by the page breaker.
This should take up two pages."
  }

  \paper {
    between-title-spacing = #'((padding . 300))
  }

  \markup "00"
  \markup "01"
  { c' }
}
