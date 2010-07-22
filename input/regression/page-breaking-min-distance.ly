\version "2.13.22"

\header {
  texidoc = "minimum-distance is correctly accounted for in page breaking."
}

\book {
  \paper {
    between-scores-system-spacing #'minimum-distance = #'20
    paper-height = 8\cm
  }

  \score { c'1 }
  \score { c'1 }
  \score { c'1 }
  \score { c'1 }
}

