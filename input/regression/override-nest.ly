\version "2.7.32"
\header {

  texidoc = "Sublist of grob property lists may be also tuned. In the
next example, the @code{beamed-lengths} property of the @code{Stem}
grob is tweaked."

}

\relative {
  \override Stem #'details #'beamed-lengths = #'(8 8 8)
  c8[ c]
}
