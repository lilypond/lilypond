\version "2.16.0"

#(set-default-paper-size "a6")

\book {
  \header {
    texidoc = "Padding between markups is honored by the page
breaker.  This should take up two pages."
  }

  \paper {
    markup-markup-spacing = #'((padding . 300))
  }

  \markup "00"
  \markup "01"
  { c' }
}
