\version "2.13.36"

#(set-default-paper-size "a6")

\book {
  \header {
    texidoc = "Padding after titles is honoured by the page breaker.
This should take up two pages."
  }

  \paper {
    markup-system-spacing = #'((padding . 300))
  }

  \markup "00"
  \markup "01"
  { c' }
}
