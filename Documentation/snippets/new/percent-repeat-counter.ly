\version "2.25.35"

\header {
  categories = "Repeats"

  texidoc = "
Measure repeats of more than two repeats are printed with a counter if
the @code{countPercentRepeats} context property is set.
"

  doctitle = "Percent repeat counter"
} % begin verbatim


\relative c'' {
  \set countPercentRepeats = ##t
  \%4 c1
}
