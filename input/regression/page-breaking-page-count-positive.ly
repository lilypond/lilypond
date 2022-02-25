\version "2.23.3"

\header {
  texidoc = "A warning is emitted when @code{page-count} is
negative or zero."
}

#(ly:set-option 'warning-as-error)
#(ly:expect-warning (G_ "page-count must be positive"))

\paper {
  page-count = 0
}

\book {
  { s }
}
