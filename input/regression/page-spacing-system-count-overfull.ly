\version "2.13.4"

\header {
  texidoc = "Page breaking doesn't crash when the line-breaking
is invalid."
}

\book {
  \paper {
    system-count = #1
  }

  \repeat unfold 20 { c d e f }
}
