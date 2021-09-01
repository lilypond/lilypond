\version "2.23.4"

\header {
  texidoc = "Test markup commands used for conditional constructs.
See also @file{markup-conditionals-several-pages.ly}."
}

\book {
  \paper {
    #(set-paper-size "a7landscape")
    print-all-headers = ##t
    oddHeaderMarkup = \markup \column {
      \if \single-page "Printed because there is a single page."
      \if \should-print-all-headers \line {
        Also printed, because \typewriter print-all-headers is true.
      }
    }
  }
  { c' }
}
