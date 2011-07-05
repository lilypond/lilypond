\version "2.14.0"

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
