\version "2.27.5"

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning (ly:translate-cpp-warning-scheme
                    "cannot fit the music into %d systems; using %d")
  1 2)

#(set-default-paper-size "a5")

\header {
  texidoc = "Page breaking doesn't crash when the line-breaking
is invalid."
}

\book {
  \paper {
    system-count = #1
  }

  \*10 { c d e f }
}
