\version "2.19.12"

\header {
  texidoc = "minimum-distance is correctly accounted for in page breaking."
}

\book {
  \paper {
    score-system-spacing.minimum-distance = #'20
    paper-height = 8\cm
  }

  \score { c'1 }
  \score { c'1 }
  \score { c'1 }
  \score { c'1 }
}

