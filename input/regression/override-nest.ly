\version "2.11.51"

\header {
  texidoc = "Sublist of grob property lists may be also tuned.  In the
next example, the @code{beamed-lengths} property of the @code{Stem}
grob is tweaked."
}

\relative {
  \override Stem #'details #'beamed-lengths = #'(6 10 8)
  c8[ c] c16[ c] c32[ c]
  \revert Stem #'details
  c8[ c] c16[ c] c32[ c]
}
