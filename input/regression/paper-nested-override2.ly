\version "2.13.42"

\header {
  texidoc = "Setting individual nested paper properties does not
remove existing settings or break spacing annotation."
}

\book {
  \paper {
    annotate-spacing = ##t
    system-system-spacing = #'((basic-distance . 12) (minimum-distance . 8))
    system-system-spacing #'padding = #1
  }
  \relative c' {
    \repeat unfold 10 { a4 d e f }
  }
}
