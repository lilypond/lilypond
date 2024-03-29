\version "2.16.0"

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning (G_ "cannot find line breaking that satisfies constraints"))

#(set-default-paper-size "a5")

\header {
  texidoc = "Page breaking doesn't crash when the line-breaking
is invalid."
}

\book {
  \paper {
    system-count = #1
  }

  \repeat unfold 10 { c d e f }
}
