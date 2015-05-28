\version "2.19.21"

\header {
  texidoc = "Setting individual nested paper properties does not
remove existing settings or break spacing annotation."
}

\book {
  \paper {
    annotate-spacing = ##t
    system-system-spacing = #'((basic-distance . 12) (minimum-distance . 8))
    system-system-spacing.padding = #1
  }
  \relative {
    \repeat unfold 10 { a4 d e f }
  }
}
